# encoding: utf-8

require 'erb'
require 'fileutils'

module RubyProf
  # prints a HTML visualization of the call tree
  class CallStackPrinter < AbstractPrinter
    include ERB::Util

    # Specify print options.
    #
    # options - Hash table
    #   :min_percent - Number 0 to 100 that specifes the minimum
    #                  %self (the methods self time divided by the
    #                  overall total time) that a method must take
    #                  for it to be printed out in the report.
    #                  Default value is 0.
    #
    #   :print_file  - True or false. Specifies if a method's source
    #                  file should be printed.  Default value if false.
    #
    #   :threshold   - a float from 0 to 100 that sets the threshold of
    #                  results displayed.
    #                  Default value is 1.0
    #
    #   :title       - a String to overide the default "ruby-prof call tree"
    #                  title of the report.
    #
    #   :expansion   - a float from 0 to 100 that sets the threshold of
    #                  results that are expanded, if the percent_total
    #                  exceeds it.
    #                  Default value is 10.0
    #
    #   :application - a String to overide the name of the application,
    #                  as it appears on the report.
    #
    def print(output = STDOUT, options = {})
      @output = output
      setup_options(options)
      if @graph_html = options.delete(:graph)
        @graph_html = "file://" + @graph_html if @graph_html[0]=="/"
      end

      print_header

      @overall_threads_time = @result.threads.inject(0) do |val, thread|
        val += thread.total_time
      end

      @result.threads.each do |thread|
        @current_thread_id = thread.fiber_id
        @overall_time = thread.total_time
        thread_info = "Thread: #{thread.id}"
        thread_info << ", Fiber: #{thread.fiber_id}" unless thread.id == thread.fiber_id
        thread_info << " (#{"%4.2f%%" % ((@overall_time/@overall_threads_time)*100)} ~ #{@overall_time})"
        @output.print "<div class=\"thread\">#{thread_info}</div>"
        @output.print "<ul name=\"thread\">"
        thread.methods.each do |m|
          # $stderr.print m.dump
          next unless m.root?
          m.call_infos.each do |ci|
            next unless ci.root?
            print_stack ci, thread.total_time
          end
        end
        @output.print "</ul>"
      end

      print_footer

      copy_image_files
    end

    def print_stack(call_info, parent_time)
      total_time = call_info.total_time
      percent_parent = (total_time/parent_time)*100
      percent_total = (total_time/@overall_time)*100
      return unless percent_total > min_percent
      color = self.color(percent_total)
      kids = call_info.children
      visible = percent_total >= threshold
      expanded = percent_total >= expansion
      display = visible ? "block" : "none"
      @output.print "<li class=\"color#{color}\" style=\"display:#{display}\">"
      if kids.empty?
        @output.print "<img src=\"empty.png\">"
      else
        visible_children = kids.any?{|ci| (ci.total_time/@overall_time)*100 >= threshold}
        image = visible_children ? (expanded ? "minus" : "plus") : "empty"
        @output.print "<img class=\"toggle\" src=\"#{image}.png\">"
      end
      @output.printf " %4.2f%% (%4.2f%%) %s %s\n", percent_total, percent_parent, link(call_info), graph_link(call_info)
      unless kids.empty?
        if expanded
          @output.print "<ul>"
        else
          @output.print '<ul style="display:none">'
        end
        kids.sort_by{|c| -c.total_time}.each do |callinfo|
          print_stack callinfo, total_time
        end
        @output.print "</ul>"
      end
      @output.print "</li>"
    end

    def name(call_info)
      method = call_info.target
      method.full_name
    end

    def link(call_info)
      method = call_info.target
      file = File.expand_path(method.source_file)
      if file =~ /\/ruby_runtime$/
        h(name(call_info))
      else
        if RUBY_PLATFORM =~ /darwin/
          "<a href=\"txmt://open?url=file://#{file}&line=#{method.line}\">#{h(name(call_info))}</a>"
        else
          "<a href=\"file://#{file}##{method.line}\">#{h(name(call_info))}</a>"
        end
      end
    end

    def graph_link(call_info)
      total_calls = call_info.target.call_infos.inject(0){|t, ci| t += ci.called}
      href = "#{@graph_html}##{method_href(call_info.target)}"
      totals = @graph_html ? "<a href='#{href}'>#{total_calls}</a>" : total_calls.to_s
      "[#{call_info.called} calls, #{totals} total]"
    end

    def method_href(method)
      h(method.full_name.gsub(/[><#\.\?=:]/,"_") + "_" + @current_thread_id.to_s)
    end

    def total_time(call_infos)
      sum(call_infos.map{|ci| ci.total_time})
    end

    def sum(a)
      a.inject(0.0){|s,t| s+=t}
    end

    def dump(ci)
      $stderr.printf "%s/%d t:%f s:%f w:%f  \n", ci, ci.object_id, ci.total_time, ci.self_time, ci.wait_time
    end

    def color(p)
      case i = p.to_i
      when 0..5
        "01"
      when 5..10
        "05"
      when 100
        "9"
      else
        "#{i/10}"
      end
    end

    def application
      @options[:application] || $PROGRAM_NAME
    end

    def arguments
      ARGV.join(' ')
    end

    def title
      @title ||= @options.delete(:title) || "ruby-prof call tree"
    end

    def threshold
      @options[:threshold] || 1.0
    end

    def expansion
      @options[:expansion] || 10.0
    end

    def copy_image_files
      if @output.is_a?(File)
        target_dir = File.dirname(@output.path)
        image_dir = File.join(File.dirname(__FILE__), '..', 'images')
        %w(empty plus minus).each do |img|
          source_file = "#{image_dir}/#{img}.png"
          target_file = "#{target_dir}/#{img}.png"
          FileUtils.cp(source_file, target_file) unless File.exist?(target_file)
        end
      end
    end

    def print_header
      @output.puts "<html><head>"
      @output.puts '<meta http-equiv="content-type" content="text/html; charset=utf-8">'
      @output.puts "<title>#{h title}</title>"
      print_css
      print_java_script
      @output.puts '</head><body><div style="display: inline-block;">'
      print_title_bar
      print_commands
      print_help
    end

    def print_footer
      @output.puts '<div id="sentinel"></div></div></body></html>'
    end

    def print_css
      @output.puts <<-'end_css'
<style type="text/css">
<!--
body {
    font-size:70%;
    padding:0px;
    margin:5px;
    margin-right:0px;
    margin-left:0px;
    background: #ffffff;
}
ul {
    margin-left:0px;
    margin-top:0px;
    margin-bottom:0px;
    padding-left:0px;
    list-style-type:none;
}
li {
    margin-left:11px;
    padding:0px;
    white-space:nowrap;
    border-top:1px solid #cccccc;
    border-left:1px solid #cccccc;
    border-bottom:none;
}
.thread {
    margin-left:11px;
    background:#708090;
    padding-top:3px;
    padding-left:12px;
    padding-bottom:2px;
    border-left:1px solid #CCCCCC;
    border-top:1px solid #CCCCCC;
    font-weight:bold;
}
.hidden {
    display:none;
    width:0px;
    height:0px;
    margin:0px;
    padding:0px;
    border-style:none;
}
.color01 { background:#adbdeb }
.color05 { background:#9daddb }
.color0 { background:#8d9dcb }
.color1 { background:#89bccb }
.color2 { background:#56e3e7 }
.color3 { background:#32cd70 }
.color4 { background:#a3d53c }
.color5 { background:#c4cb34 }
.color6 { background:#dcb66d }
.color7 { background:#cda59e }
.color8 { background:#be9d9c }
.color9 { background:#cf947a }
#commands {
    font-size:10pt;
    padding:10px;
    margin-left:11px;
    margin-bottom:0px;
    margin-top:0px;
    background:#708090;
    border-top:1px solid #cccccc;
    border-left:1px solid #cccccc;
    border-bottom:none;
}
#titlebar {
    font-size:10pt;
    padding:10px;
    margin-left:11px;
    margin-bottom:0px;
    margin-top:10px;
    background:#8090a0;
    border-top:1px solid #cccccc;
    border-left:1px solid #cccccc;
    border-bottom:none;
}
#help {
    font-size:10pt;
    padding:10px;
    margin-left:11px;
    margin-bottom:0px;
    margin-top:0px;
    background:#8090a0;
    display:none;
    border-top:1px solid #cccccc;
    border-left:1px solid #cccccc;
    border-bottom:none;
}
#sentinel {
    height: 400px;
    margin-left:11px;
    background:#8090a0;
    border-top:1px solid #cccccc;
    border-left:1px solid #cccccc;
    border-bottom:none;
 }
input { margin-left:10px; }
-->
</style>
end_css
    end

    def print_java_script
      @output.puts <<-'end_java_script'
<script type="text/javascript">
/*
   Copyright (C) 2005,2009  Stefan Kaes
   skaes@railsexpress.de
*/

function rootNode() {
  return currentThread;
}

function hideUL(node) {
  var lis = node.childNodes
  var l = lis.length;
  for (var i=0; i < l ; i++ ) {
    hideLI(lis[i]);
  }
}

function showUL(node) {
  var lis = node.childNodes;
  var l = lis.length;
  for (var i=0; i < l ; i++ ) {
    showLI(lis[i]);
  }
}

function findUlChild(li){
  var ul = li.childNodes[2];
  while (ul && ul.nodeName != "UL") {
    ul = ul.nextSibling;
  }
  return ul;
}

function isLeafNode(li) {
  var img = li.firstChild;
  return (img.src.indexOf('empty.png') > -1);
}

function hideLI(li) {
  if (isLeafNode(li))
    return;

  var img = li.firstChild;
  img.src = 'plus.png';

  var ul = findUlChild(li);
  if (ul) {
    ul.style.display = 'none';
    hideUL(ul);
  }
}

function showLI(li) {
  if (isLeafNode(li))
    return;

  var img = li.firstChild;
  img.src = 'minus.png';

  var ul = findUlChild(li);
  if (ul) {
    ul.style.display = 'block';
    showUL(ul);
  }
}

function toggleLI(li) {
  var img = li.firstChild;
  if (img.src.indexOf("minus.png")>-1)
    hideLI(li);
  else {
    if (img.src.indexOf("plus.png")>-1)
      showLI(li);
  }
}

function aboveThreshold(text, threshold) {
  var match = text.match(/\d+[.,]\d+/);
  return (match && parseFloat(match[0].replace(/,/, '.'))>=threshold);
}

function setThresholdLI(li, threshold) {
  var img = li.firstChild;
  var text = img.nextSibling;
  var ul = findUlChild(li);

  var visible = aboveThreshold(text.nodeValue, threshold) ? 1 : 0;

  var count = 0;
  if (ul) {
    count = setThresholdUL(ul, threshold);
  }
  if (count>0) {
    img.src = 'minus.png';
  }
  else {
    img.src = 'empty.png';
  }
  if (visible) {
    li.style.display = 'block'
  }
  else {
    li.style.display = 'none'
  }
  return visible;
}

function setThresholdUL(node, threshold) {
  var lis = node.childNodes;
  var l = lis.length;

  var count = 0;
  for ( var i = 0; i < l ; i++ ) {
    count = count + setThresholdLI(lis[i], threshold);
  }

  var visible = (count > 0) ? 1 : 0;
  if (visible) {
    node.style.display = 'block';
  }
  else {
    node.style.display = 'none';
  }
  return visible;
}

function toggleChildren(img, event) {
  event.cancelBubble=true;

  if (img.src.indexOf('empty.png') > -1)
    return;

  var minus = (img.src.indexOf('minus.png') > -1);

  if (minus) {
    img.src = 'plus.png';
  }
  else
    img.src = 'minus.png';

  var li = img.parentNode;
  var ul = findUlChild(li);
  if (ul) {
    if (minus)
      ul.style.display = 'none';
    else
      ul.style.display = 'block';
  }
  if (minus)
    moveSelectionIfNecessary(li);
}

function showChildren(li) {
  var img = li.firstChild;
  if (img.src.indexOf('empty.png') > -1)
    return;
  img.src = 'minus.png';

  var ul = findUlChild(li);
  if (ul) {
    ul.style.display = 'block';
  }
}

function setThreshold() {
 var tv = document.getElementById("threshold").value;
 if (tv.match(/[0-9]+([.,][0-9]+)?/)) {
   var f = parseFloat(tv.replace(/,/, '.'));
   var threads = document.getElementsByName("thread");
   var l = threads.length;
   for ( var i = 0; i < l ; i++ ) {
     setThresholdUL(threads[i], f);
   }
   var p = selectedNode;
   while (p && p.style.display=='none')
     p=p.parentNode.parentNode;
   if (p && p.nodeName=="LI")
    selectNode(p);
 }
 else {
   alert("Please specify a decimal number as threshold value!");
 }
}

function collapseAll(event) {
  event.cancelBubble=true;
  var threads = document.getElementsByName("thread");
  var l = threads.length;
  for ( var i = 0; i < l ; i++ ) {
    hideUL(threads[i]);
  }
  selectNode(rootNode(), null);
}

function expandAll(event) {
  event.cancelBubble=true;
  var threads = document.getElementsByName("thread");
  var l = threads.length;
  for ( var i = 0; i < l ; i++ ) {
    showUL(threads[i]);
  }
}

function toggleHelp(node) {
  var help = document.getElementById("help");
  if (node.value == "Show Help") {
    node.value = "Hide Help";
    help.style.display = 'block';
  }
  else {
    node.value = "Show Help";
    help.style.display = 'none';
  }
}

var selectedNode = null;
var selectedColor = null;
var selectedThread = null;

function descendentOf(a,b){
  while (a!=b && b!=null)
    b=b.parentNode;
  return (a==b);
}

function moveSelectionIfNecessary(node){
  if (descendentOf(node, selectedNode))
    selectNode(node, null);
}

function selectNode(node, event) {
  if (event) {
    event.cancelBubble = true;
    thread = findThread(node);
    selectThread(thread);
  }
  if (selectedNode) {
    selectedNode.style.background = selectedColor;
  }
  selectedNode = node;
  selectedColor = node.style.background;
  selectedNode.style.background = "red";
  selectedNode.scrollIntoView();
  window.scrollBy(0,-400);
}

function moveUp(){
  var p = selectedNode.previousSibling;
  while (p && p.style.display == 'none')
    p = p.previousSibling;
  if (p && p.nodeName == "LI") {
    selectNode(p, null);
  }
}

function moveDown(){
  var p = selectedNode.nextSibling;
  while (p && p.style.display == 'none')
    p = p.nextSibling;
  if (p && p.nodeName == "LI") {
    selectNode(p, null);
  }
}

function moveLeft(){
  var p = selectedNode.parentNode.parentNode;
  if (p && p.nodeName=="LI") {
    selectNode(p, null);
  }
}

function moveRight(){
  if (!isLeafNode(selectedNode)) {
    showChildren(selectedNode);
    var ul = findUlChild(selectedNode);
    if (ul) {
      selectNode(ul.firstChild, null);
    }
  }
}

function moveForward(){
  if (isLeafNode(selectedNode)) {
    var p = selectedNode;
    while ((p.nextSibling == null || p.nextSibling.style.display=='none') && p.nodeName=="LI") {
      p = p.parentNode.parentNode;
    }
    if (p.nodeName=="LI")
      selectNode(p.nextSibling, null);
  }
  else {
    moveRight();
  }
}

function isExpandedNode(li){
  var img = li.firstChild;
  return(img.src.indexOf('minus.png')>-1);
}

function moveBackward(){
  var p = selectedNode;
  var q = p.previousSibling;
  while (q != null && q.style.display=='none')
    q = q.previousSibling;
  if (q == null) {
    p = p.parentNode.parentNode;
  } else {
    while (!isLeafNode(q) && isExpandedNode(q)) {
      q = findUlChild(q).lastChild;
      while (q.style.display=='none')
        q = q.previousSibling;
    }
    p = q;
  }
  if (p.nodeName=="LI")
    selectNode(p, null);
}

function moveHome() {
  selectNode(currentThread);
}

var currentThreadIndex = null;

function findThread(node){
  while (node && node.parentNode.nodeName!="BODY") {
    node = node.parentNode;
  }
  return node.firstChild;
}

function selectThread(node){
  var threads = document.getElementsByName("thread");
  currentThread = node;
  for (var i=0; i<threads.length; i++) {
    if (threads[i]==currentThread.parentNode)
      currentThreadIndex = i;
  }
}

function nextThread(){
  var threads = document.getElementsByName("thread");
  if (currentThreadIndex==threads.length-1)
    currentThreadIndex = 0;
  else
    currentThreadIndex += 1
  currentThread = threads[currentThreadIndex].firstChild;
  selectNode(currentThread, null);
}

function previousThread(){
  var threads = document.getElementsByName("thread");
  if (currentThreadIndex==0)
    currentThreadIndex = threads.length-1;
  else
    currentThreadIndex -= 1
  currentThread = threads[currentThreadIndex].firstChild;
  selectNode(currentThread, null);
}

function switchThread(node, event){
  event.cancelBubble = true;
  selectThread(node.nextSibling.firstChild);
  selectNode(currentThread, null);
}

function handleKeyEvent(event){
  var code = event.charCode ? event.charCode : event.keyCode;
  var str = String.fromCharCode(code);
  switch (str) {
    case "a": moveLeft();  break;
    case "s": moveDown();  break;
    case "d": moveRight(); break;
    case "w": moveUp();    break;
    case "f": moveForward(); break;
    case "b": moveBackward(); break;
    case "x": toggleChildren(selectedNode.firstChild, event); break;
    case "*": toggleLI(selectedNode); break;
    case "n": nextThread(); break;
    case "h": moveHome(); break;
    case "p": previousThread(); break;
  }
}
document.onkeypress=function(event){ handleKeyEvent(event) };

window.onload=function(){
  var images = document.getElementsByTagName("img");
  for (var i=0; i<images.length; i++) {
    var img = images[i];
    if (img.className == "toggle") {
      img.onclick = function(event){ toggleChildren(this, event); };
    }
  }
  var divs = document.getElementsByTagName("div");
  for (i=0; i<divs.length; i++) {
    var div = divs[i];
    if (div.className == "thread")
      div.onclick = function(event){ switchThread(this, event) };
  }
  var lis = document.getElementsByTagName("li");
  for (var i=0; i<lis.length; i++) {
    lis[i].onclick = function(event){ selectNode(this, event); };
  }
  var threads = document.getElementsByName("thread");
  currentThreadIndex = 0;
  currentThread = threads[0].firstChild;
  selectNode(currentThread, null);
}
</script>
end_java_script
    end

    def print_title_bar
      @output.puts <<-"end_title_bar"
<div id="titlebar">
Call tree for application <b>#{h application} #{h arguments}</b><br/>
Generated on #{Time.now} with options #{h @options.inspect}<br/>
</div>
end_title_bar
    end

    def print_commands
      @output.puts <<-"end_commands"
<div id=\"commands\">
<span style=\"font-size: 11pt; font-weight: bold;\">Threshold:</span>
<input value=\"#{h threshold}\" size=\"3\" id=\"threshold\" type=\"text\">
<input value=\"Apply\" onclick=\"setThreshold();\" type=\"submit\">
<input value=\"Expand All\" onclick=\"expandAll(event);\" type=\"submit\">
<input value=\"Collapse All\" onclick=\"collapseAll(event);\" type=\"submit\">
<input value=\"Show Help\" onclick=\"toggleHelp(this);\" type=\"submit\">
</div>
end_commands
    end

    def print_help
      @output.puts <<-'end_help'
<div style="display: none;" id="help">
<img src="empty.png"> Enter a decimal value <i>d</i> into the threshold field and click "Apply"
to hide all nodes marked with time values lower than <i>d</i>.<br>
<img src="empty.png"> Click on "Expand All" for full tree expansion.<br>
<img src="empty.png"> Click on "Collapse All" to show only top level nodes.<br>
<img src="empty.png"> Use a, s, d, w as in Quake or Urban Terror to navigate the tree.<br>
<img src="empty.png"> Use f and b to navigate the tree in preorder forward and backwards.<br>
<img src="empty.png"> Use x to toggle visibility of a subtree.<br>
<img src="empty.png"> Use * to expand/collapse a whole subtree.<br>
<img src="empty.png"> Use h to navigate to thread root.<br>
<img src="empty.png"> Use n and p to navigate between threads.<br>
<img src="empty.png"> Click on background to move focus to a subtree.<br>
</div>
end_help
    end
  end
end

