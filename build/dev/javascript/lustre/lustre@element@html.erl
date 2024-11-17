-module(lustre@element@html).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([html/2, text/1, base/1, head/2, link/1, meta/1, style/2, title/2, body/2, address/2, article/2, aside/2, footer/2, header/2, h1/2, h2/2, h3/2, h4/2, h5/2, h6/2, hgroup/2, main/2, nav/2, section/2, search/2, blockquote/2, dd/2, 'div'/2, dl/2, dt/2, figcaption/2, figure/2, hr/1, li/2, menu/2, ol/2, p/2, pre/2, ul/2, a/2, abbr/2, b/2, bdi/2, bdo/2, br/1, cite/2, code/2, data/2, dfn/2, em/2, i/2, kbd/2, mark/2, q/2, rp/2, rt/2, ruby/2, s/2, samp/2, small/2, span/2, strong/2, sub/2, sup/2, time/2, u/2, var/2, wbr/1, area/1, audio/2, img/1, map/2, track/1, video/2, embed/1, iframe/1, object/1, picture/2, portal/1, source/1, svg/2, math/2, canvas/1, noscript/2, script/2, del/2, ins/2, caption/2, col/1, colgroup/2, table/2, tbody/2, td/2, tfoot/2, th/2, thead/2, tr/2, button/2, datalist/2, fieldset/2, form/2, input/1, label/2, legend/2, meter/2, optgroup/2, option/2, output/2, progress/2, select/2, textarea/2, details/2, dialog/2, summary/2, slot/1, template/2]).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 9).
-spec html(
    list(lustre@internals@vdom:attribute(QXW)),
    list(lustre@internals@vdom:element(QXW))
) -> lustre@internals@vdom:element(QXW).
html(Attrs, Children) ->
    lustre@element:element(<<"html"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 16).
-spec text(binary()) -> lustre@internals@vdom:element(any()).
text(Content) ->
    lustre@element:text(Content).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 23).
-spec base(list(lustre@internals@vdom:attribute(QYE))) -> lustre@internals@vdom:element(QYE).
base(Attrs) ->
    lustre@element:element(<<"base"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 28).
-spec head(
    list(lustre@internals@vdom:attribute(QYI)),
    list(lustre@internals@vdom:element(QYI))
) -> lustre@internals@vdom:element(QYI).
head(Attrs, Children) ->
    lustre@element:element(<<"head"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 36).
-spec link(list(lustre@internals@vdom:attribute(QYO))) -> lustre@internals@vdom:element(QYO).
link(Attrs) ->
    lustre@element:element(<<"link"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 41).
-spec meta(list(lustre@internals@vdom:attribute(QYS))) -> lustre@internals@vdom:element(QYS).
meta(Attrs) ->
    lustre@element:element(<<"meta"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 46).
-spec style(list(lustre@internals@vdom:attribute(QYW)), binary()) -> lustre@internals@vdom:element(QYW).
style(Attrs, Css) ->
    lustre@element:element(<<"style"/utf8>>, Attrs, [text(Css)]).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 51).
-spec title(list(lustre@internals@vdom:attribute(QZA)), binary()) -> lustre@internals@vdom:element(QZA).
title(Attrs, Content) ->
    lustre@element:element(<<"title"/utf8>>, Attrs, [text(Content)]).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 58).
-spec body(
    list(lustre@internals@vdom:attribute(QZE)),
    list(lustre@internals@vdom:element(QZE))
) -> lustre@internals@vdom:element(QZE).
body(Attrs, Children) ->
    lustre@element:element(<<"body"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 68).
-spec address(
    list(lustre@internals@vdom:attribute(QZK)),
    list(lustre@internals@vdom:element(QZK))
) -> lustre@internals@vdom:element(QZK).
address(Attrs, Children) ->
    lustre@element:element(<<"address"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 76).
-spec article(
    list(lustre@internals@vdom:attribute(QZQ)),
    list(lustre@internals@vdom:element(QZQ))
) -> lustre@internals@vdom:element(QZQ).
article(Attrs, Children) ->
    lustre@element:element(<<"article"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 84).
-spec aside(
    list(lustre@internals@vdom:attribute(QZW)),
    list(lustre@internals@vdom:element(QZW))
) -> lustre@internals@vdom:element(QZW).
aside(Attrs, Children) ->
    lustre@element:element(<<"aside"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 92).
-spec footer(
    list(lustre@internals@vdom:attribute(RAC)),
    list(lustre@internals@vdom:element(RAC))
) -> lustre@internals@vdom:element(RAC).
footer(Attrs, Children) ->
    lustre@element:element(<<"footer"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 100).
-spec header(
    list(lustre@internals@vdom:attribute(RAI)),
    list(lustre@internals@vdom:element(RAI))
) -> lustre@internals@vdom:element(RAI).
header(Attrs, Children) ->
    lustre@element:element(<<"header"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 108).
-spec h1(
    list(lustre@internals@vdom:attribute(RAO)),
    list(lustre@internals@vdom:element(RAO))
) -> lustre@internals@vdom:element(RAO).
h1(Attrs, Children) ->
    lustre@element:element(<<"h1"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 116).
-spec h2(
    list(lustre@internals@vdom:attribute(RAU)),
    list(lustre@internals@vdom:element(RAU))
) -> lustre@internals@vdom:element(RAU).
h2(Attrs, Children) ->
    lustre@element:element(<<"h2"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 124).
-spec h3(
    list(lustre@internals@vdom:attribute(RBA)),
    list(lustre@internals@vdom:element(RBA))
) -> lustre@internals@vdom:element(RBA).
h3(Attrs, Children) ->
    lustre@element:element(<<"h3"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 132).
-spec h4(
    list(lustre@internals@vdom:attribute(RBG)),
    list(lustre@internals@vdom:element(RBG))
) -> lustre@internals@vdom:element(RBG).
h4(Attrs, Children) ->
    lustre@element:element(<<"h4"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 140).
-spec h5(
    list(lustre@internals@vdom:attribute(RBM)),
    list(lustre@internals@vdom:element(RBM))
) -> lustre@internals@vdom:element(RBM).
h5(Attrs, Children) ->
    lustre@element:element(<<"h5"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 148).
-spec h6(
    list(lustre@internals@vdom:attribute(RBS)),
    list(lustre@internals@vdom:element(RBS))
) -> lustre@internals@vdom:element(RBS).
h6(Attrs, Children) ->
    lustre@element:element(<<"h6"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 156).
-spec hgroup(
    list(lustre@internals@vdom:attribute(RBY)),
    list(lustre@internals@vdom:element(RBY))
) -> lustre@internals@vdom:element(RBY).
hgroup(Attrs, Children) ->
    lustre@element:element(<<"hgroup"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 164).
-spec main(
    list(lustre@internals@vdom:attribute(RCE)),
    list(lustre@internals@vdom:element(RCE))
) -> lustre@internals@vdom:element(RCE).
main(Attrs, Children) ->
    lustre@element:element(<<"main"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 172).
-spec nav(
    list(lustre@internals@vdom:attribute(RCK)),
    list(lustre@internals@vdom:element(RCK))
) -> lustre@internals@vdom:element(RCK).
nav(Attrs, Children) ->
    lustre@element:element(<<"nav"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 180).
-spec section(
    list(lustre@internals@vdom:attribute(RCQ)),
    list(lustre@internals@vdom:element(RCQ))
) -> lustre@internals@vdom:element(RCQ).
section(Attrs, Children) ->
    lustre@element:element(<<"section"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 188).
-spec search(
    list(lustre@internals@vdom:attribute(RCW)),
    list(lustre@internals@vdom:element(RCW))
) -> lustre@internals@vdom:element(RCW).
search(Attrs, Children) ->
    lustre@element:element(<<"search"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 198).
-spec blockquote(
    list(lustre@internals@vdom:attribute(RDC)),
    list(lustre@internals@vdom:element(RDC))
) -> lustre@internals@vdom:element(RDC).
blockquote(Attrs, Children) ->
    lustre@element:element(<<"blockquote"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 206).
-spec dd(
    list(lustre@internals@vdom:attribute(RDI)),
    list(lustre@internals@vdom:element(RDI))
) -> lustre@internals@vdom:element(RDI).
dd(Attrs, Children) ->
    lustre@element:element(<<"dd"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 214).
-spec 'div'(
    list(lustre@internals@vdom:attribute(RDO)),
    list(lustre@internals@vdom:element(RDO))
) -> lustre@internals@vdom:element(RDO).
'div'(Attrs, Children) ->
    lustre@element:element(<<"div"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 222).
-spec dl(
    list(lustre@internals@vdom:attribute(RDU)),
    list(lustre@internals@vdom:element(RDU))
) -> lustre@internals@vdom:element(RDU).
dl(Attrs, Children) ->
    lustre@element:element(<<"dl"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 230).
-spec dt(
    list(lustre@internals@vdom:attribute(REA)),
    list(lustre@internals@vdom:element(REA))
) -> lustre@internals@vdom:element(REA).
dt(Attrs, Children) ->
    lustre@element:element(<<"dt"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 238).
-spec figcaption(
    list(lustre@internals@vdom:attribute(REG)),
    list(lustre@internals@vdom:element(REG))
) -> lustre@internals@vdom:element(REG).
figcaption(Attrs, Children) ->
    lustre@element:element(<<"figcaption"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 246).
-spec figure(
    list(lustre@internals@vdom:attribute(REM)),
    list(lustre@internals@vdom:element(REM))
) -> lustre@internals@vdom:element(REM).
figure(Attrs, Children) ->
    lustre@element:element(<<"figure"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 254).
-spec hr(list(lustre@internals@vdom:attribute(RES))) -> lustre@internals@vdom:element(RES).
hr(Attrs) ->
    lustre@element:element(<<"hr"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 259).
-spec li(
    list(lustre@internals@vdom:attribute(REW)),
    list(lustre@internals@vdom:element(REW))
) -> lustre@internals@vdom:element(REW).
li(Attrs, Children) ->
    lustre@element:element(<<"li"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 267).
-spec menu(
    list(lustre@internals@vdom:attribute(RFC)),
    list(lustre@internals@vdom:element(RFC))
) -> lustre@internals@vdom:element(RFC).
menu(Attrs, Children) ->
    lustre@element:element(<<"menu"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 275).
-spec ol(
    list(lustre@internals@vdom:attribute(RFI)),
    list(lustre@internals@vdom:element(RFI))
) -> lustre@internals@vdom:element(RFI).
ol(Attrs, Children) ->
    lustre@element:element(<<"ol"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 283).
-spec p(
    list(lustre@internals@vdom:attribute(RFO)),
    list(lustre@internals@vdom:element(RFO))
) -> lustre@internals@vdom:element(RFO).
p(Attrs, Children) ->
    lustre@element:element(<<"p"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 291).
-spec pre(
    list(lustre@internals@vdom:attribute(RFU)),
    list(lustre@internals@vdom:element(RFU))
) -> lustre@internals@vdom:element(RFU).
pre(Attrs, Children) ->
    lustre@element:element(<<"pre"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 299).
-spec ul(
    list(lustre@internals@vdom:attribute(RGA)),
    list(lustre@internals@vdom:element(RGA))
) -> lustre@internals@vdom:element(RGA).
ul(Attrs, Children) ->
    lustre@element:element(<<"ul"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 309).
-spec a(
    list(lustre@internals@vdom:attribute(RGG)),
    list(lustre@internals@vdom:element(RGG))
) -> lustre@internals@vdom:element(RGG).
a(Attrs, Children) ->
    lustre@element:element(<<"a"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 317).
-spec abbr(
    list(lustre@internals@vdom:attribute(RGM)),
    list(lustre@internals@vdom:element(RGM))
) -> lustre@internals@vdom:element(RGM).
abbr(Attrs, Children) ->
    lustre@element:element(<<"abbr"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 325).
-spec b(
    list(lustre@internals@vdom:attribute(RGS)),
    list(lustre@internals@vdom:element(RGS))
) -> lustre@internals@vdom:element(RGS).
b(Attrs, Children) ->
    lustre@element:element(<<"b"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 333).
-spec bdi(
    list(lustre@internals@vdom:attribute(RGY)),
    list(lustre@internals@vdom:element(RGY))
) -> lustre@internals@vdom:element(RGY).
bdi(Attrs, Children) ->
    lustre@element:element(<<"bdi"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 341).
-spec bdo(
    list(lustre@internals@vdom:attribute(RHE)),
    list(lustre@internals@vdom:element(RHE))
) -> lustre@internals@vdom:element(RHE).
bdo(Attrs, Children) ->
    lustre@element:element(<<"bdo"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 349).
-spec br(list(lustre@internals@vdom:attribute(RHK))) -> lustre@internals@vdom:element(RHK).
br(Attrs) ->
    lustre@element:element(<<"br"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 354).
-spec cite(
    list(lustre@internals@vdom:attribute(RHO)),
    list(lustre@internals@vdom:element(RHO))
) -> lustre@internals@vdom:element(RHO).
cite(Attrs, Children) ->
    lustre@element:element(<<"cite"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 362).
-spec code(
    list(lustre@internals@vdom:attribute(RHU)),
    list(lustre@internals@vdom:element(RHU))
) -> lustre@internals@vdom:element(RHU).
code(Attrs, Children) ->
    lustre@element:element(<<"code"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 370).
-spec data(
    list(lustre@internals@vdom:attribute(RIA)),
    list(lustre@internals@vdom:element(RIA))
) -> lustre@internals@vdom:element(RIA).
data(Attrs, Children) ->
    lustre@element:element(<<"data"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 378).
-spec dfn(
    list(lustre@internals@vdom:attribute(RIG)),
    list(lustre@internals@vdom:element(RIG))
) -> lustre@internals@vdom:element(RIG).
dfn(Attrs, Children) ->
    lustre@element:element(<<"dfn"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 386).
-spec em(
    list(lustre@internals@vdom:attribute(RIM)),
    list(lustre@internals@vdom:element(RIM))
) -> lustre@internals@vdom:element(RIM).
em(Attrs, Children) ->
    lustre@element:element(<<"em"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 394).
-spec i(
    list(lustre@internals@vdom:attribute(RIS)),
    list(lustre@internals@vdom:element(RIS))
) -> lustre@internals@vdom:element(RIS).
i(Attrs, Children) ->
    lustre@element:element(<<"i"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 402).
-spec kbd(
    list(lustre@internals@vdom:attribute(RIY)),
    list(lustre@internals@vdom:element(RIY))
) -> lustre@internals@vdom:element(RIY).
kbd(Attrs, Children) ->
    lustre@element:element(<<"kbd"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 410).
-spec mark(
    list(lustre@internals@vdom:attribute(RJE)),
    list(lustre@internals@vdom:element(RJE))
) -> lustre@internals@vdom:element(RJE).
mark(Attrs, Children) ->
    lustre@element:element(<<"mark"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 418).
-spec q(
    list(lustre@internals@vdom:attribute(RJK)),
    list(lustre@internals@vdom:element(RJK))
) -> lustre@internals@vdom:element(RJK).
q(Attrs, Children) ->
    lustre@element:element(<<"q"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 426).
-spec rp(
    list(lustre@internals@vdom:attribute(RJQ)),
    list(lustre@internals@vdom:element(RJQ))
) -> lustre@internals@vdom:element(RJQ).
rp(Attrs, Children) ->
    lustre@element:element(<<"rp"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 434).
-spec rt(
    list(lustre@internals@vdom:attribute(RJW)),
    list(lustre@internals@vdom:element(RJW))
) -> lustre@internals@vdom:element(RJW).
rt(Attrs, Children) ->
    lustre@element:element(<<"rt"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 442).
-spec ruby(
    list(lustre@internals@vdom:attribute(RKC)),
    list(lustre@internals@vdom:element(RKC))
) -> lustre@internals@vdom:element(RKC).
ruby(Attrs, Children) ->
    lustre@element:element(<<"ruby"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 450).
-spec s(
    list(lustre@internals@vdom:attribute(RKI)),
    list(lustre@internals@vdom:element(RKI))
) -> lustre@internals@vdom:element(RKI).
s(Attrs, Children) ->
    lustre@element:element(<<"s"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 458).
-spec samp(
    list(lustre@internals@vdom:attribute(RKO)),
    list(lustre@internals@vdom:element(RKO))
) -> lustre@internals@vdom:element(RKO).
samp(Attrs, Children) ->
    lustre@element:element(<<"samp"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 466).
-spec small(
    list(lustre@internals@vdom:attribute(RKU)),
    list(lustre@internals@vdom:element(RKU))
) -> lustre@internals@vdom:element(RKU).
small(Attrs, Children) ->
    lustre@element:element(<<"small"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 474).
-spec span(
    list(lustre@internals@vdom:attribute(RLA)),
    list(lustre@internals@vdom:element(RLA))
) -> lustre@internals@vdom:element(RLA).
span(Attrs, Children) ->
    lustre@element:element(<<"span"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 482).
-spec strong(
    list(lustre@internals@vdom:attribute(RLG)),
    list(lustre@internals@vdom:element(RLG))
) -> lustre@internals@vdom:element(RLG).
strong(Attrs, Children) ->
    lustre@element:element(<<"strong"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 490).
-spec sub(
    list(lustre@internals@vdom:attribute(RLM)),
    list(lustre@internals@vdom:element(RLM))
) -> lustre@internals@vdom:element(RLM).
sub(Attrs, Children) ->
    lustre@element:element(<<"sub"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 498).
-spec sup(
    list(lustre@internals@vdom:attribute(RLS)),
    list(lustre@internals@vdom:element(RLS))
) -> lustre@internals@vdom:element(RLS).
sup(Attrs, Children) ->
    lustre@element:element(<<"sup"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 506).
-spec time(
    list(lustre@internals@vdom:attribute(RLY)),
    list(lustre@internals@vdom:element(RLY))
) -> lustre@internals@vdom:element(RLY).
time(Attrs, Children) ->
    lustre@element:element(<<"time"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 514).
-spec u(
    list(lustre@internals@vdom:attribute(RME)),
    list(lustre@internals@vdom:element(RME))
) -> lustre@internals@vdom:element(RME).
u(Attrs, Children) ->
    lustre@element:element(<<"u"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 522).
-spec var(
    list(lustre@internals@vdom:attribute(RMK)),
    list(lustre@internals@vdom:element(RMK))
) -> lustre@internals@vdom:element(RMK).
var(Attrs, Children) ->
    lustre@element:element(<<"var"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 530).
-spec wbr(list(lustre@internals@vdom:attribute(RMQ))) -> lustre@internals@vdom:element(RMQ).
wbr(Attrs) ->
    lustre@element:element(<<"wbr"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 537).
-spec area(list(lustre@internals@vdom:attribute(RMU))) -> lustre@internals@vdom:element(RMU).
area(Attrs) ->
    lustre@element:element(<<"area"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 542).
-spec audio(
    list(lustre@internals@vdom:attribute(RMY)),
    list(lustre@internals@vdom:element(RMY))
) -> lustre@internals@vdom:element(RMY).
audio(Attrs, Children) ->
    lustre@element:element(<<"audio"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 550).
-spec img(list(lustre@internals@vdom:attribute(RNE))) -> lustre@internals@vdom:element(RNE).
img(Attrs) ->
    lustre@element:element(<<"img"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 556).
-spec map(
    list(lustre@internals@vdom:attribute(RNI)),
    list(lustre@internals@vdom:element(RNI))
) -> lustre@internals@vdom:element(RNI).
map(Attrs, Children) ->
    lustre@element:element(<<"map"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 564).
-spec track(list(lustre@internals@vdom:attribute(RNO))) -> lustre@internals@vdom:element(RNO).
track(Attrs) ->
    lustre@element:element(<<"track"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 569).
-spec video(
    list(lustre@internals@vdom:attribute(RNS)),
    list(lustre@internals@vdom:element(RNS))
) -> lustre@internals@vdom:element(RNS).
video(Attrs, Children) ->
    lustre@element:element(<<"video"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 579).
-spec embed(list(lustre@internals@vdom:attribute(RNY))) -> lustre@internals@vdom:element(RNY).
embed(Attrs) ->
    lustre@element:element(<<"embed"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 584).
-spec iframe(list(lustre@internals@vdom:attribute(ROC))) -> lustre@internals@vdom:element(ROC).
iframe(Attrs) ->
    lustre@element:element(<<"iframe"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 589).
-spec object(list(lustre@internals@vdom:attribute(ROG))) -> lustre@internals@vdom:element(ROG).
object(Attrs) ->
    lustre@element:element(<<"object"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 594).
-spec picture(
    list(lustre@internals@vdom:attribute(ROK)),
    list(lustre@internals@vdom:element(ROK))
) -> lustre@internals@vdom:element(ROK).
picture(Attrs, Children) ->
    lustre@element:element(<<"picture"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 602).
-spec portal(list(lustre@internals@vdom:attribute(ROQ))) -> lustre@internals@vdom:element(ROQ).
portal(Attrs) ->
    lustre@element:element(<<"portal"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 607).
-spec source(list(lustre@internals@vdom:attribute(ROU))) -> lustre@internals@vdom:element(ROU).
source(Attrs) ->
    lustre@element:element(<<"source"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 614).
-spec svg(
    list(lustre@internals@vdom:attribute(ROY)),
    list(lustre@internals@vdom:element(ROY))
) -> lustre@internals@vdom:element(ROY).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 622).
-spec math(
    list(lustre@internals@vdom:attribute(RPE)),
    list(lustre@internals@vdom:element(RPE))
) -> lustre@internals@vdom:element(RPE).
math(Attrs, Children) ->
    lustre@element:element(<<"math"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 632).
-spec canvas(list(lustre@internals@vdom:attribute(RPK))) -> lustre@internals@vdom:element(RPK).
canvas(Attrs) ->
    lustre@element:element(<<"canvas"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 637).
-spec noscript(
    list(lustre@internals@vdom:attribute(RPO)),
    list(lustre@internals@vdom:element(RPO))
) -> lustre@internals@vdom:element(RPO).
noscript(Attrs, Children) ->
    lustre@element:element(<<"noscript"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 645).
-spec script(list(lustre@internals@vdom:attribute(RPU)), binary()) -> lustre@internals@vdom:element(RPU).
script(Attrs, Js) ->
    lustre@element:element(<<"script"/utf8>>, Attrs, [text(Js)]).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 652).
-spec del(
    list(lustre@internals@vdom:attribute(RPY)),
    list(lustre@internals@vdom:element(RPY))
) -> lustre@internals@vdom:element(RPY).
del(Attrs, Children) ->
    lustre@element:element(<<"del"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 660).
-spec ins(
    list(lustre@internals@vdom:attribute(RQE)),
    list(lustre@internals@vdom:element(RQE))
) -> lustre@internals@vdom:element(RQE).
ins(Attrs, Children) ->
    lustre@element:element(<<"ins"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 670).
-spec caption(
    list(lustre@internals@vdom:attribute(RQK)),
    list(lustre@internals@vdom:element(RQK))
) -> lustre@internals@vdom:element(RQK).
caption(Attrs, Children) ->
    lustre@element:element(<<"caption"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 678).
-spec col(list(lustre@internals@vdom:attribute(RQQ))) -> lustre@internals@vdom:element(RQQ).
col(Attrs) ->
    lustre@element:element(<<"col"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 683).
-spec colgroup(
    list(lustre@internals@vdom:attribute(RQU)),
    list(lustre@internals@vdom:element(RQU))
) -> lustre@internals@vdom:element(RQU).
colgroup(Attrs, Children) ->
    lustre@element:element(<<"colgroup"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 691).
-spec table(
    list(lustre@internals@vdom:attribute(RRA)),
    list(lustre@internals@vdom:element(RRA))
) -> lustre@internals@vdom:element(RRA).
table(Attrs, Children) ->
    lustre@element:element(<<"table"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 699).
-spec tbody(
    list(lustre@internals@vdom:attribute(RRG)),
    list(lustre@internals@vdom:element(RRG))
) -> lustre@internals@vdom:element(RRG).
tbody(Attrs, Children) ->
    lustre@element:element(<<"tbody"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 707).
-spec td(
    list(lustre@internals@vdom:attribute(RRM)),
    list(lustre@internals@vdom:element(RRM))
) -> lustre@internals@vdom:element(RRM).
td(Attrs, Children) ->
    lustre@element:element(<<"td"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 715).
-spec tfoot(
    list(lustre@internals@vdom:attribute(RRS)),
    list(lustre@internals@vdom:element(RRS))
) -> lustre@internals@vdom:element(RRS).
tfoot(Attrs, Children) ->
    lustre@element:element(<<"tfoot"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 723).
-spec th(
    list(lustre@internals@vdom:attribute(RRY)),
    list(lustre@internals@vdom:element(RRY))
) -> lustre@internals@vdom:element(RRY).
th(Attrs, Children) ->
    lustre@element:element(<<"th"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 731).
-spec thead(
    list(lustre@internals@vdom:attribute(RSE)),
    list(lustre@internals@vdom:element(RSE))
) -> lustre@internals@vdom:element(RSE).
thead(Attrs, Children) ->
    lustre@element:element(<<"thead"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 739).
-spec tr(
    list(lustre@internals@vdom:attribute(RSK)),
    list(lustre@internals@vdom:element(RSK))
) -> lustre@internals@vdom:element(RSK).
tr(Attrs, Children) ->
    lustre@element:element(<<"tr"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 749).
-spec button(
    list(lustre@internals@vdom:attribute(RSQ)),
    list(lustre@internals@vdom:element(RSQ))
) -> lustre@internals@vdom:element(RSQ).
button(Attrs, Children) ->
    lustre@element:element(<<"button"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 757).
-spec datalist(
    list(lustre@internals@vdom:attribute(RSW)),
    list(lustre@internals@vdom:element(RSW))
) -> lustre@internals@vdom:element(RSW).
datalist(Attrs, Children) ->
    lustre@element:element(<<"datalist"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 765).
-spec fieldset(
    list(lustre@internals@vdom:attribute(RTC)),
    list(lustre@internals@vdom:element(RTC))
) -> lustre@internals@vdom:element(RTC).
fieldset(Attrs, Children) ->
    lustre@element:element(<<"fieldset"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 773).
-spec form(
    list(lustre@internals@vdom:attribute(RTI)),
    list(lustre@internals@vdom:element(RTI))
) -> lustre@internals@vdom:element(RTI).
form(Attrs, Children) ->
    lustre@element:element(<<"form"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 781).
-spec input(list(lustre@internals@vdom:attribute(RTO))) -> lustre@internals@vdom:element(RTO).
input(Attrs) ->
    lustre@element:element(<<"input"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 786).
-spec label(
    list(lustre@internals@vdom:attribute(RTS)),
    list(lustre@internals@vdom:element(RTS))
) -> lustre@internals@vdom:element(RTS).
label(Attrs, Children) ->
    lustre@element:element(<<"label"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 794).
-spec legend(
    list(lustre@internals@vdom:attribute(RTY)),
    list(lustre@internals@vdom:element(RTY))
) -> lustre@internals@vdom:element(RTY).
legend(Attrs, Children) ->
    lustre@element:element(<<"legend"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 802).
-spec meter(
    list(lustre@internals@vdom:attribute(RUE)),
    list(lustre@internals@vdom:element(RUE))
) -> lustre@internals@vdom:element(RUE).
meter(Attrs, Children) ->
    lustre@element:element(<<"meter"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 810).
-spec optgroup(
    list(lustre@internals@vdom:attribute(RUK)),
    list(lustre@internals@vdom:element(RUK))
) -> lustre@internals@vdom:element(RUK).
optgroup(Attrs, Children) ->
    lustre@element:element(<<"optgroup"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 818).
-spec option(list(lustre@internals@vdom:attribute(RUQ)), binary()) -> lustre@internals@vdom:element(RUQ).
option(Attrs, Label) ->
    lustre@element:element(
        <<"option"/utf8>>,
        Attrs,
        [lustre@element:text(Label)]
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 823).
-spec output(
    list(lustre@internals@vdom:attribute(RUU)),
    list(lustre@internals@vdom:element(RUU))
) -> lustre@internals@vdom:element(RUU).
output(Attrs, Children) ->
    lustre@element:element(<<"output"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 831).
-spec progress(
    list(lustre@internals@vdom:attribute(RVA)),
    list(lustre@internals@vdom:element(RVA))
) -> lustre@internals@vdom:element(RVA).
progress(Attrs, Children) ->
    lustre@element:element(<<"progress"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 839).
-spec select(
    list(lustre@internals@vdom:attribute(RVG)),
    list(lustre@internals@vdom:element(RVG))
) -> lustre@internals@vdom:element(RVG).
select(Attrs, Children) ->
    lustre@element:element(<<"select"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 847).
-spec textarea(list(lustre@internals@vdom:attribute(RVM)), binary()) -> lustre@internals@vdom:element(RVM).
textarea(Attrs, Content) ->
    lustre@element:element(
        <<"textarea"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 854).
-spec details(
    list(lustre@internals@vdom:attribute(RVQ)),
    list(lustre@internals@vdom:element(RVQ))
) -> lustre@internals@vdom:element(RVQ).
details(Attrs, Children) ->
    lustre@element:element(<<"details"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 862).
-spec dialog(
    list(lustre@internals@vdom:attribute(RVW)),
    list(lustre@internals@vdom:element(RVW))
) -> lustre@internals@vdom:element(RVW).
dialog(Attrs, Children) ->
    lustre@element:element(<<"dialog"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 870).
-spec summary(
    list(lustre@internals@vdom:attribute(RWC)),
    list(lustre@internals@vdom:element(RWC))
) -> lustre@internals@vdom:element(RWC).
summary(Attrs, Children) ->
    lustre@element:element(<<"summary"/utf8>>, Attrs, Children).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 880).
-spec slot(list(lustre@internals@vdom:attribute(RWI))) -> lustre@internals@vdom:element(RWI).
slot(Attrs) ->
    lustre@element:element(<<"slot"/utf8>>, Attrs, []).

-file("/Users/hayleigh/work/lustre-labs/lustre/src/lustre/element/html.gleam", 885).
-spec template(
    list(lustre@internals@vdom:attribute(RWM)),
    list(lustre@internals@vdom:element(RWM))
) -> lustre@internals@vdom:element(RWM).
template(Attrs, Children) ->
    lustre@element:element(<<"template"/utf8>>, Attrs, Children).
