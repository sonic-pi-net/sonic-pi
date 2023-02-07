defmodule Tau.HydraSynthLangTest do
  use ExUnit.Case
  doctest Tau.HydraSynthLang

  test "simple osc" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("osc().out()") ===
             "hydra.osc().out()"
  end

  test "simple osc with accidental spaces" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("osc   ().out()") ===
             "hydra.osc().out()"
  end

  test "simple multiline osc" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("osc().
    out()") === "hydra.osc().
    out()"
  end

  test "simple noise" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("noise().out()") ===
             "hydra.noise().out()"
  end

  test "simple render" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("render(o0)") ===
             "hydra.render(hydra.o0)"
  end

  test "noise 0" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
    noise(10, 0.1).out(o0)") === "
    hydra.noise(10, 0.1).out(hydra.o0)"
  end

  test "noise 1" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
    noise( () => Math.sin(time/10)*50 , () => Math.sin(time/2)/500 )
.out(o0)") === "
    hydra.noise( () => Math.sin(hydra.time/10)*50 , () => Math.sin(hydra.time/2)/500 )
.out(hydra.o0)"
  end

  test "voronoi 0" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
    voronoi(5,0.3,0.3).out(o0)") === "
    hydra.voronoi(5,0.3,0.3).out(hydra.o0)"
  end

  test "voronoi 1" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
    voronoi(25,2,10).color(1,1,0).brightness(0.15).out(o0)") === "
    hydra.voronoi(25,2,10).color(1,1,0).brightness(0.15).out(hydra.o0)"
  end

  test "osc 0" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
    osc( () => Math.sin(time/10) * 100 ).out(o0)") === "
    hydra.osc( () => Math.sin(hydra.time/10) * 100 ).out(hydra.o0)"
  end

  test "osc 1" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
    osc( [1,10,50,100,250,500].fast(2) ).out(o0)") === "
    hydra.osc( [1,10,50,100,250,500].fast(2) ).out(hydra.o0)"
  end

  test "osc 3" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
    osc(10,0.1, ({time}) => Math.sin(time/10) * 100 ).out(o0)") === "
    hydra.osc(10,0.1, ({___time___}) => Math.sin(hydra.time/10) * 100 ).out(hydra.o0)"
  end

  test "shape 0" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
    shape(3,0.5,0.001).out(o0)") === "
    hydra.shape(3,0.5,0.001).out(hydra.o0)"
  end

  test "shape 2" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
    shape(100,0.01,1).invert(()=>Math.sin(time)*2).out(o0)") === "
    hydra.shape(100,0.01,1).invert(()=>Math.sin(hydra.time)*2).out(hydra.o0)"
  end

  test "shape 3" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
    shape(5,0.5,0.1).repeat(19,19)
    .mult(osc(10,1,2))
    .rotate( ({time}) => time%360 )
    .scrollX(1,-0.25)
    .mult(shape(15,0.3,0.01)
    .rotate( ({time}) => time%360 )
    .scrollX(1,-0.25))
    .out(o0)") === "
    hydra.shape(5,0.5,0.1).repeat(19,19)
    .mult(hydra.osc(10,1,2))
    .rotate( ({___time___}) => hydra.time%360 )
    .scrollX(1,-0.25)
    .mult(hydra.shape(15,0.3,0.01)
    .rotate( ({___time___}) => hydra.time%360 )
    .scrollX(1,-0.25))
    .out(hydra.o0)"
  end

  test "gradiant 0" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
    gradient([1,2,4]).out(o0)") === "
    hydra.gradient([1,2,4]).out(hydra.o0)"
  end

  test "gradiant 1" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
    gradient(0).r().repeat(16,1).scrollX(0,0.1).out(o0)") === "
    hydra.gradient(0).r().repeat(16,1).scrollX(0,0.1).out(hydra.o0)"
  end

  # please test for compatibility:
  #
  #   noise(3,0.1,7)
  # .rotate(1,-1,-2).mask(shape(20))
  # .colorama(0.5)
  # .modulateScale(o0)
  # .modulateScale(o0,1,)
  # .blend(o0)
  # .blend(o0)
  # .blend(o0)
  # .blend(o0)
  # .out(o0)

  test "full example 2" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance(
             "osc(15, 0.01, 0.1).mult(osc(1, -0.1).modulate(osc(2).rotate(4,1), 20))
             .color(0,2.4,5)
             .saturate(0.4)
             .luma(1,0.1, (6, ()=> 1 + a.fft[3]))
             .scale(0.7, ()=> 12.7 + a.fft[3])
             .diff(o0) // o0
             .out(o0)// o1"
           ) ===
             "hydra.osc(15, 0.01, 0.1).mult(hydra.osc(1, -0.1).modulate(hydra.osc(2).rotate(4,1), 20))
             .color(0,2.4,5)
             .saturate(0.4)
             .luma(1,0.1, (6, ()=> 1 + hydra.a.fft[3]))
             .scale(0.7, ()=> 12.7 + hydra.a.fft[3])
             .diff(hydra.o0) // hydra.o0
             .out(hydra.o0)// hydra.o1"
  end

  test "full example 4" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
             osc(30,0.01,1)
             .mult(osc(20,-0.1,1).modulate(noise(3,1)).rotate(0.7))
             .posterize([3,10,2].fast(0.5).smooth(1))
             .modulateRotate(o0,()=>mouse.x*0.003)
             .out()") ===
             "
             hydra.osc(30,0.01,1)
             .mult(hydra.osc(20,-0.1,1).modulate(hydra.noise(3,1)).rotate(0.7))
             .posterize([3,10,2].fast(0.5).smooth(1))
             .modulateRotate(hydra.o0,()=>hydra.mouse.x*0.003)
             .out()"
  end

  test "full example 3" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
      osc(40,0.2,1)
      .modulateScale(osc(40,0,1).kaleid(8))
      .repeat(2,4)
      .modulate(o0,0.05)
      .modulateKaleid(shape(4,0.1,1))
      .out(o0)") === "
      hydra.osc(40,0.2,1)
      .modulateScale(hydra.osc(40,0,1).kaleid(8))
      .repeat(2,4)
      .modulate(hydra.o0,0.05)
      .modulateKaleid(hydra.shape(4,0.1,1))
      .out(hydra.o0)"
  end

  test "full example 5" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("
      noise(3,0.3,3).thresh(0.3,0.03).diff(o3,0.3).out(o1)
      gradient([0.3,0.3,3]).diff(o0).blend(o1).out(o3)
      voronoi(33,3,30).rotate(3,0.3,0).modulateScale(o2,0.3).color(-3,3,0).brightness(3).out(o0)
      shape(30,0.3,1).invert(({time})=>Math.sin(time)*3).out(o2)

      render(o3)") === "
      hydra.noise(3,0.3,3).thresh(0.3,0.03).diff(hydra.o3,0.3).out(hydra.o1)
      hydra.gradient([0.3,0.3,3]).diff(hydra.o0).blend(hydra.o1).out(hydra.o3)
      hydra.voronoi(33,3,30).rotate(3,0.3,0).modulateScale(hydra.o2,0.3).color(-3,3,0).brightness(3).out(hydra.o0)
      hydra.shape(30,0.3,1).invert(({___time___})=>Math.sin(hydra.time)*3).out(hydra.o2)

      hydra.render(hydra.o3)"
  end

  test "full example 1" do
    assert Tau.HydraSynthLang.convert_global_syntax_to_instance("noise().
    thresh(0.5).
    rotate(
      () => time * 0.1
    ).
    kaleid(3).
    add(
      src(o0).
      scale(0.9)
    ).
    rotate(
      () => time * 0.1
    ).
    sub(
      src(o0).
      scale(1.1)
    ).
    thresh(0.5).
    mult(
      osc(1,0.1,Math.PI/4)
    ).
    out()") === "hydra.noise().
    thresh(0.5).
    rotate(
      () => hydra.time * 0.1
    ).
    kaleid(3).
    add(
      hydra.src(hydra.o0).
      scale(0.9)
    ).
    rotate(
      () => hydra.time * 0.1
    ).
    sub(
      hydra.src(hydra.o0).
      scale(1.1)
    ).
    thresh(0.5).
    mult(
      hydra.osc(1,0.1,Math.PI/4)
    ).
    out()"
  end
end
