-module(lustre@element@svg).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([animate/1, animate_motion/1, animate_transform/1, mpath/1, set/1, circle/1, ellipse/1, line/1, polygon/1, polyline/1, rect/1, a/2, defs/2, g/2, marker/2, mask/2, missing_glyph/2, pattern/2, svg/2, switch/2, symbol/2, desc/2, metadata/2, title/2, fe_blend/1, fe_color_matrix/1, fe_component_transfer/1, fe_composite/1, fe_convolve_matrix/1, fe_diffuse_lighting/2, fe_displacement_map/1, fe_drop_shadow/1, fe_flood/1, fe_func_a/1, fe_func_b/1, fe_func_g/1, fe_func_r/1, fe_gaussian_blur/1, fe_image/1, fe_merge/2, fe_merge_node/1, fe_morphology/1, fe_offset/1, fe_specular_lighting/2, fe_tile/2, fe_turbulence/1, linear_gradient/2, radial_gradient/2, stop/1, image/1, path/1, text/2, use_/1, fe_distant_light/1, fe_point_light/1, fe_spot_light/1, clip_path/2, script/2, style/2, foreign_object/2, text_path/2, tspan/2]).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 19).
-spec animate(list(lustre@internals@vdom:attribute(SDP))) -> lustre@internals@vdom:element(SDP).
animate(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animate"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 24).
-spec animate_motion(list(lustre@internals@vdom:attribute(SDT))) -> lustre@internals@vdom:element(SDT).
animate_motion(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateMotion"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 29).
-spec animate_transform(list(lustre@internals@vdom:attribute(SDX))) -> lustre@internals@vdom:element(SDX).
animate_transform(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateTransform"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 34).
-spec mpath(list(lustre@internals@vdom:attribute(SEB))) -> lustre@internals@vdom:element(SEB).
mpath(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mpath"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 39).
-spec set(list(lustre@internals@vdom:attribute(SEF))) -> lustre@internals@vdom:element(SEF).
set(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"set"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 46).
-spec circle(list(lustre@internals@vdom:attribute(SEJ))) -> lustre@internals@vdom:element(SEJ).
circle(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"circle"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 51).
-spec ellipse(list(lustre@internals@vdom:attribute(SEN))) -> lustre@internals@vdom:element(SEN).
ellipse(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"ellipse"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 56).
-spec line(list(lustre@internals@vdom:attribute(SER))) -> lustre@internals@vdom:element(SER).
line(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"line"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 61).
-spec polygon(list(lustre@internals@vdom:attribute(SEV))) -> lustre@internals@vdom:element(SEV).
polygon(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polygon"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 66).
-spec polyline(list(lustre@internals@vdom:attribute(SEZ))) -> lustre@internals@vdom:element(SEZ).
polyline(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polyline"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 71).
-spec rect(list(lustre@internals@vdom:attribute(SFD))) -> lustre@internals@vdom:element(SFD).
rect(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"rect"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 78).
-spec a(
    list(lustre@internals@vdom:attribute(SFH)),
    list(lustre@internals@vdom:element(SFH))
) -> lustre@internals@vdom:element(SFH).
a(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"a"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 86).
-spec defs(
    list(lustre@internals@vdom:attribute(SFN)),
    list(lustre@internals@vdom:element(SFN))
) -> lustre@internals@vdom:element(SFN).
defs(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"defs"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 94).
-spec g(
    list(lustre@internals@vdom:attribute(SFT)),
    list(lustre@internals@vdom:element(SFT))
) -> lustre@internals@vdom:element(SFT).
g(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"g"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 102).
-spec marker(
    list(lustre@internals@vdom:attribute(SFZ)),
    list(lustre@internals@vdom:element(SFZ))
) -> lustre@internals@vdom:element(SFZ).
marker(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"marker"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 110).
-spec mask(
    list(lustre@internals@vdom:attribute(SGF)),
    list(lustre@internals@vdom:element(SGF))
) -> lustre@internals@vdom:element(SGF).
mask(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mask"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 118).
-spec missing_glyph(
    list(lustre@internals@vdom:attribute(SGL)),
    list(lustre@internals@vdom:element(SGL))
) -> lustre@internals@vdom:element(SGL).
missing_glyph(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"missing-glyph"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 126).
-spec pattern(
    list(lustre@internals@vdom:attribute(SGR)),
    list(lustre@internals@vdom:element(SGR))
) -> lustre@internals@vdom:element(SGR).
pattern(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"pattern"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 134).
-spec svg(
    list(lustre@internals@vdom:attribute(SGX)),
    list(lustre@internals@vdom:element(SGX))
) -> lustre@internals@vdom:element(SGX).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 142).
-spec switch(
    list(lustre@internals@vdom:attribute(SHD)),
    list(lustre@internals@vdom:element(SHD))
) -> lustre@internals@vdom:element(SHD).
switch(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"switch"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 150).
-spec symbol(
    list(lustre@internals@vdom:attribute(SHJ)),
    list(lustre@internals@vdom:element(SHJ))
) -> lustre@internals@vdom:element(SHJ).
symbol(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"symbol"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 160).
-spec desc(
    list(lustre@internals@vdom:attribute(SHP)),
    list(lustre@internals@vdom:element(SHP))
) -> lustre@internals@vdom:element(SHP).
desc(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"desc"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 168).
-spec metadata(
    list(lustre@internals@vdom:attribute(SHV)),
    list(lustre@internals@vdom:element(SHV))
) -> lustre@internals@vdom:element(SHV).
metadata(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"metadata"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 176).
-spec title(
    list(lustre@internals@vdom:attribute(SIB)),
    list(lustre@internals@vdom:element(SIB))
) -> lustre@internals@vdom:element(SIB).
title(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"title"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 186).
-spec fe_blend(list(lustre@internals@vdom:attribute(SIH))) -> lustre@internals@vdom:element(SIH).
fe_blend(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feBlend"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 191).
-spec fe_color_matrix(list(lustre@internals@vdom:attribute(SIL))) -> lustre@internals@vdom:element(SIL).
fe_color_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feColorMatrix"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 196).
-spec fe_component_transfer(list(lustre@internals@vdom:attribute(SIP))) -> lustre@internals@vdom:element(SIP).
fe_component_transfer(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComponentTransfer"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 201).
-spec fe_composite(list(lustre@internals@vdom:attribute(SIT))) -> lustre@internals@vdom:element(SIT).
fe_composite(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComposite"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 206).
-spec fe_convolve_matrix(list(lustre@internals@vdom:attribute(SIX))) -> lustre@internals@vdom:element(SIX).
fe_convolve_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feConvolveMatrix"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 211).
-spec fe_diffuse_lighting(
    list(lustre@internals@vdom:attribute(SJB)),
    list(lustre@internals@vdom:element(SJB))
) -> lustre@internals@vdom:element(SJB).
fe_diffuse_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDiffuseLighting"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 219).
-spec fe_displacement_map(list(lustre@internals@vdom:attribute(SJH))) -> lustre@internals@vdom:element(SJH).
fe_displacement_map(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDisplacementMap"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 224).
-spec fe_drop_shadow(list(lustre@internals@vdom:attribute(SJL))) -> lustre@internals@vdom:element(SJL).
fe_drop_shadow(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDropShadow"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 229).
-spec fe_flood(list(lustre@internals@vdom:attribute(SJP))) -> lustre@internals@vdom:element(SJP).
fe_flood(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFlood"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 234).
-spec fe_func_a(list(lustre@internals@vdom:attribute(SJT))) -> lustre@internals@vdom:element(SJT).
fe_func_a(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncA"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 239).
-spec fe_func_b(list(lustre@internals@vdom:attribute(SJX))) -> lustre@internals@vdom:element(SJX).
fe_func_b(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncB"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 244).
-spec fe_func_g(list(lustre@internals@vdom:attribute(SKB))) -> lustre@internals@vdom:element(SKB).
fe_func_g(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncG"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 249).
-spec fe_func_r(list(lustre@internals@vdom:attribute(SKF))) -> lustre@internals@vdom:element(SKF).
fe_func_r(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncR"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 254).
-spec fe_gaussian_blur(list(lustre@internals@vdom:attribute(SKJ))) -> lustre@internals@vdom:element(SKJ).
fe_gaussian_blur(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feGaussianBlur"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 259).
-spec fe_image(list(lustre@internals@vdom:attribute(SKN))) -> lustre@internals@vdom:element(SKN).
fe_image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feImage"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 264).
-spec fe_merge(
    list(lustre@internals@vdom:attribute(SKR)),
    list(lustre@internals@vdom:element(SKR))
) -> lustre@internals@vdom:element(SKR).
fe_merge(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMerge"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 272).
-spec fe_merge_node(list(lustre@internals@vdom:attribute(SKX))) -> lustre@internals@vdom:element(SKX).
fe_merge_node(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMergeNode"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 277).
-spec fe_morphology(list(lustre@internals@vdom:attribute(SLB))) -> lustre@internals@vdom:element(SLB).
fe_morphology(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMorphology"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 282).
-spec fe_offset(list(lustre@internals@vdom:attribute(SLF))) -> lustre@internals@vdom:element(SLF).
fe_offset(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feOffset"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 287).
-spec fe_specular_lighting(
    list(lustre@internals@vdom:attribute(SLJ)),
    list(lustre@internals@vdom:element(SLJ))
) -> lustre@internals@vdom:element(SLJ).
fe_specular_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpecularLighting"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 295).
-spec fe_tile(
    list(lustre@internals@vdom:attribute(SLP)),
    list(lustre@internals@vdom:element(SLP))
) -> lustre@internals@vdom:element(SLP).
fe_tile(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTile"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 303).
-spec fe_turbulence(list(lustre@internals@vdom:attribute(SLV))) -> lustre@internals@vdom:element(SLV).
fe_turbulence(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTurbulence"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 310).
-spec linear_gradient(
    list(lustre@internals@vdom:attribute(SLZ)),
    list(lustre@internals@vdom:element(SLZ))
) -> lustre@internals@vdom:element(SLZ).
linear_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"linearGradient"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 318).
-spec radial_gradient(
    list(lustre@internals@vdom:attribute(SMF)),
    list(lustre@internals@vdom:element(SMF))
) -> lustre@internals@vdom:element(SMF).
radial_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"radialGradient"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 326).
-spec stop(list(lustre@internals@vdom:attribute(SML))) -> lustre@internals@vdom:element(SML).
stop(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"stop"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 333).
-spec image(list(lustre@internals@vdom:attribute(SMP))) -> lustre@internals@vdom:element(SMP).
image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"image"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 338).
-spec path(list(lustre@internals@vdom:attribute(SMT))) -> lustre@internals@vdom:element(SMT).
path(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"path"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 343).
-spec text(list(lustre@internals@vdom:attribute(SMX)), binary()) -> lustre@internals@vdom:element(SMX).
text(Attrs, Content) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"text"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 348).
-spec use_(list(lustre@internals@vdom:attribute(SNB))) -> lustre@internals@vdom:element(SNB).
use_(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"use"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 355).
-spec fe_distant_light(list(lustre@internals@vdom:attribute(SNF))) -> lustre@internals@vdom:element(SNF).
fe_distant_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDistantLight"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 360).
-spec fe_point_light(list(lustre@internals@vdom:attribute(SNJ))) -> lustre@internals@vdom:element(SNJ).
fe_point_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"fePointLight"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 365).
-spec fe_spot_light(list(lustre@internals@vdom:attribute(SNN))) -> lustre@internals@vdom:element(SNN).
fe_spot_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpotLight"/utf8>>,
        Attrs,
        []
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 372).
-spec clip_path(
    list(lustre@internals@vdom:attribute(SNR)),
    list(lustre@internals@vdom:element(SNR))
) -> lustre@internals@vdom:element(SNR).
clip_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"clipPath"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 380).
-spec script(list(lustre@internals@vdom:attribute(SNX)), binary()) -> lustre@internals@vdom:element(SNX).
script(Attrs, Js) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"script"/utf8>>,
        Attrs,
        [lustre@element:text(Js)]
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 385).
-spec style(list(lustre@internals@vdom:attribute(SOB)), binary()) -> lustre@internals@vdom:element(SOB).
style(Attrs, Css) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"style"/utf8>>,
        Attrs,
        [lustre@element:text(Css)]
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 392).
-spec foreign_object(
    list(lustre@internals@vdom:attribute(SOF)),
    list(lustre@internals@vdom:element(SOF))
) -> lustre@internals@vdom:element(SOF).
foreign_object(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"foreignObject"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 400).
-spec text_path(
    list(lustre@internals@vdom:attribute(SOL)),
    list(lustre@internals@vdom:element(SOL))
) -> lustre@internals@vdom:element(SOL).
text_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"textPath"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/svg.gleam", 408).
-spec tspan(
    list(lustre@internals@vdom:attribute(SOR)),
    list(lustre@internals@vdom:element(SOR))
) -> lustre@internals@vdom:element(SOR).
tspan(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"tspan"/utf8>>,
        Attrs,
        Children
    ).
