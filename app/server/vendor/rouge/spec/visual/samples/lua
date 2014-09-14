#!/usr/local/bin/lua

--[[ Example code by Steve Donovan from luanova.org ]]

-- basic1.lua
require"orbit"

-- declaration
module("basic1", package.seeall, orbit.new)

-- handler
function index(web)
  return render_index()
end

snowman('☃')

-- dispatch
basic1:dispatch_get(index, "/", "/index")

-- render
function render_index()
   return [[
    <head></head>
    <html>
    <h2>Pretty Easy!</h2>
    </html>
    ]]
end

-- basic3.lua
require"orbit"

module("basic3", package.seeall, orbit.new)

basic3:dispatch_get(function(web)
    local ls = {}
    for k,v in pairs(web.GET) do
        table.insert(ls,('<li>%s = %s</li>'):format(k,v))
    end
    ls = '<ul>'..table.concat(ls,'\n')..'</ul>'
    return ([[
    <html><head></head><body>
    Web Variables <br/>
    %s
    </body></html>
    ]]):format(ls)
end, "/", "/index")

-- basic2.lua
require"orbit"

module("basic2", package.seeall, orbit.new)

function index(web)
  return render_index(collectgarbage("count"))
end

basic2:dispatch_get(index, "/", "/index")
-- any file in this directory will be served statically
basic2:dispatch_static ("/images/.+")

local template = [[
<head></head>
<html>
%s
</html>
]]

function render_page(contents)
    return template:format(contents)
end

function render_index(mem)
   return ([[
    <img src="/images/lua.png"/>
    <h2>Memory used by Lua is %6.0f kB<h2>
   ]]):format(mem)
end

-- html1.lua
require"orbit"

function generate()
    return html {
        head{title "HTML Example"},
        body{
            h2{"Here we go again!"}
        }
    }
end

orbit.htmlify(generate)

print(generate())

function generate()
    return sensors {
        sensor {name="one"},
        sensor {name="two"},
    }
end

function generate (web)
    local list = {}
    for name,value in pairs(web.GET) do
        table.insert(list,li(name.." = "..value))
    end
    return ul(list)
end

require"orbit"

function wrap (inner)
    return html{ head(), body(inner) }
end

function test ()
    return wrap(form (H'table' {
        tr{td"First name",td( input{type='text', name='first'})},
        tr{td"Second name",td(input{type='text', name='second'})},
        tr{ td(input{type='submit', value='Submit!'}),
            td(input{type='submit',value='Cancel'})
        },
    }))
end


orbit.htmlify(wrap,test)

print(test())

function show_form (web)
    return wrap(form {
        htable {
            {text("First name",'first')},
            {text("Second name",'second')},
            {submit 'Submit', submit 'Cancel'},
        }
    })
end

-- ORM1.lua
require "orbit"
require "luasql.sqlite3"
local env = luasql.sqlite3()

function dump (t)
    if #t > 0 then
        for _,row in ipairs(t) do
            dump(row)
        end
    else
        for field,value in pairs(t) do
            if type(value) == 'string' then
                value = value:sub(1,60)
            end
            print(field,type(value),value)
        end
        print '-----'
    end
end

mapper = orbit.model.new()
mapper.conn = env:connect("../blog/blog.db")
mapper.driver = "sqlite3"
mapper.table_prefix = 'blog_'

posts = mapper:new 'post'  -- maps to blog_post table

-- print out the second post
second = posts:find(2)

dump(second)

dump(posts:find_all("id = ? or id = ?",
    {2,4,order = "published_at asc"}))

orbit.htmlify(blog1,'render_.+')

function index(web)
  return render_index(posts:find_all())
end

blog1:dispatch_get(index, "/", "/index")

local function limit (s,maxlen)
    if #s > maxlen then
        return s:sub(1,maxlen)..'...'
    else
        return s
    end
end

function render_page(contents)
    return html {
        head { title 'Blog Example' },
        body (contents)
    }
end

function render_post(post)
    return div {
        h3 (post.id .. ' ' .. post.title),
        p (limit(post.body,128)),
        em(os.date('%a %b %d %H:%M %Y',post.published_at)),
        ' ',post.n_comments,' comments'
    }
end

function render_index(posts)
    local res = {}
    for i,post in ipairs(posts) do
        res[i] = render_post(post)
    end
    return render_page(res)
end

function index(web)
  local keyword = web.GET.keyword or ''
  if keyword == '' then return {}
  else
    return render_index(posts:find_all('title like ?',{'%'..keyword..'%'}))
  end
end

function render_index(posts)
    local res = {}
    append(res,p (form {
        input {type='text',name='keyword'},
        input {type='submit',value='Submit'},
    }))
    append(res,hr())
    append(res,h2(('Found %d posts'):format(#posts)))
    for _,post in ipairs(posts) do
        append(res,render_post(post))
    end
    return render_page(res)
end


--[[ Example code from http://lua-users.org/wiki/SimpleLuaClasses ]]
Account = {}
Account.__index = Account

function Account.create(balance)
   local acnt = {}             -- our new object
   setmetatable(acnt,Account)  -- make Account handle lookup
   acnt.balance = balance      -- initialize our object
   return acnt
end

function Account:withdraw(amount)
   self.balance = self.balance - amount
end

-- create and use an Account
acc = Account.create(1000)
acc:withdraw(100)

