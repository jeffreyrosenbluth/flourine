{-# LANGUAGE DeriveFunctor #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Fluorine.HTML
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- This module defines the HTML types required by the Halogen library, and provides
-- smart constructors for HTML5 elements.
-------------------------------------------------------------------------------

module Fluorine.HTML
  ( HTML(..)

  , text
  , element

  , TagName()
  , tagName
  , runTagName

  -- Elements

  , a             , a_
  , abbr          , abbr_
  , acronym       , acronym_
  , address       , address_
  , applet        , applet_
  , area          , area_
  , article       , article_
  , aside         , aside_
  , audio         , audio_
  , b             , b_
  , base          , base_
  , basefont      , basefont_
  , bdi           , bdi_
  , bdo           , bdo_
  , big           , big_
  , blockquote    , blockquote_
  , body          , body_
  , br            , br_
  , button        , button_
  , canvas        , canvas_
  , caption       , caption_
  , center        , center_
  , cite          , cite_
  , code          , code_
  , col           , col_
  , colgroup      , colgroup_
  , datalist      , datalist_
  , dd            , dd_
  , del           , del_
  , details       , details_
  , dfn           , dfn_
  , dialog        , dialog_
  , dir           , dir_
  , div           , div_
  , dl            , dl_
  , dt            , dt_
  , em            , em_
  , embed         , embed_
  , fieldset      , fieldset_
  , figcaption    , figcaption_
  , figure        , figure_
  , font          , font_
  , footer        , footer_
  , form          , form_
  , frame         , frame_
  , frameset      , frameset_
  , h1            , h1_
  , h2            , h2_
  , h3            , h3_
  , h4            , h4_
  , h5            , h5_
  , h6            , h6_
  , head          , head_
  , header        , header_
  , hr            , hr_
  , html          , html_
  , i             , i_
  , iframe        , iframe_
  , img           , img_
  , input         , input_
  , ins           , ins_
  , kbd           , kbd_
  , keygen        , keygen_
  , label         , label_
  , legend        , legend_
  , li            , li_
  , link          , link_
  , main          , main_
  , map           , map_
  , mark          , mark_
  , menu          , menu_
  , menuitem      , menuitem_
  , meta          , meta_
  , meter         , meter_
  , nav           , nav_
  , noframes      , noframes_
  , noscript      , noscript_
  , object        , object_
  , ol            , ol_
  , optgroup      , optgroup_
  , option        , option_
  , output        , output_
  , p             , p_
  , param         , param_
  , pre           , pre_
  , progress      , progress_
  , q             , q_
  , rp            , rp_
  , rt            , rt_
  , ruby          , ruby_
  , s             , s_
  , samp          , samp_
  , script        , script_
  , section       , section_
  , select        , select_
  , small         , small_
  , source        , source_
  , span          , span_
  , strike        , strike_
  , strong        , strong_
  , style         , style_
  , sub           , sub_
  , summary       , summary_
  , sup           , sup_
  , table         , table_
  , tbody         , tbody_
  , td            , td_
  , textarea      , textarea_
  , tfoot         , tfoot_
  , th            , th_
  , thead         , thead_
  , time          , time_
  , title         , title_
  , tr            , tr_
  , track         , track_
  , tt            , tt_
  , u             , u_
  , ul            , ul_
  , var           , var_
  , video         , video_
  , wbr           , wbr_
  ) where

import           Prelude hiding (div, head, map, span)
import qualified Fluorine.HTML.Attributes as A

-- | A type-safe wrapper for a HTML tag name
newtype TagName = TagName {runTagName :: String}

-- | Create a tag name
tagName :: String -> TagName
tagName = TagName

-- | An initial encoding of HTML nodes.
data HTML i
  = Text String
  | Element TagName [A.Attr i] [HTML i]
  deriving (Functor)

text :: String -> HTML i
text = Text

element :: TagName -> [A.Attr i] -> [HTML i] -> HTML i
element = Element

a :: [A.Attr i] -> [HTML i] -> HTML i
a xs = element (tagName "a") xs

a_ :: [HTML i] -> HTML i
a_ = a mempty

abbr :: [A.Attr i] -> [HTML i] -> HTML i
abbr xs = element (tagName "abbr") xs

abbr_ :: [HTML i] -> HTML i
abbr_ = abbr mempty

acronym :: [A.Attr i] -> [HTML i] -> HTML i
acronym xs = element (tagName "acronym") xs

acronym_ :: [HTML i] -> HTML i
acronym_ = acronym mempty

address :: [A.Attr i] -> [HTML i] -> HTML i
address xs = element (tagName "address") xs

address_ :: [HTML i] -> HTML i
address_ = address mempty

applet :: [A.Attr i] -> [HTML i] -> HTML i
applet xs = element (tagName "applet") xs

applet_ :: [HTML i] -> HTML i
applet_ = applet mempty

area :: [A.Attr i] -> [HTML i] -> HTML i
area xs = element (tagName "area") xs

area_ :: [HTML i] -> HTML i
area_ = area mempty

article :: [A.Attr i] -> [HTML i] -> HTML i
article xs = element (tagName "article") xs

article_ :: [HTML i] -> HTML i
article_ = article mempty

aside :: [A.Attr i] -> [HTML i] -> HTML i
aside xs = element (tagName "aside") xs

aside_ :: [HTML i] -> HTML i
aside_ = aside mempty

audio :: [A.Attr i] -> [HTML i] -> HTML i
audio xs = element (tagName "audio") xs

audio_ :: [HTML i] -> HTML i
audio_ = audio mempty

b :: [A.Attr i] -> [HTML i] -> HTML i
b xs = element (tagName "b") xs

b_ :: [HTML i] -> HTML i
b_ = b mempty

base :: [A.Attr i] -> [HTML i] -> HTML i
base xs = element (tagName "base") xs

base_ :: [HTML i] -> HTML i
base_ = base mempty

basefont :: [A.Attr i] -> [HTML i] -> HTML i
basefont xs = element (tagName "basefont") xs

basefont_ :: [HTML i] -> HTML i
basefont_ = basefont mempty

bdi :: [A.Attr i] -> [HTML i] -> HTML i
bdi xs = element (tagName "bdi") xs

bdi_ :: [HTML i] -> HTML i
bdi_ = bdi mempty

bdo :: [A.Attr i] -> [HTML i] -> HTML i
bdo xs = element (tagName "bdo") xs

bdo_ :: [HTML i] -> HTML i
bdo_ = bdo mempty

big :: [A.Attr i] -> [HTML i] -> HTML i
big xs = element (tagName "big") xs

big_ :: [HTML i] -> HTML i
big_ = big mempty

blockquote :: [A.Attr i] -> [HTML i] -> HTML i
blockquote xs = element (tagName "blockquote") xs

blockquote_ :: [HTML i] -> HTML i
blockquote_ = blockquote mempty

body :: [A.Attr i] -> [HTML i] -> HTML i
body xs = element (tagName "body") xs

body_ :: [HTML i] -> HTML i
body_ = body mempty

br :: [A.Attr i] -> [HTML i] -> HTML i
br xs = element (tagName "br") xs

br_ :: [HTML i] -> HTML i
br_ = br mempty

button :: [A.Attr i] -> [HTML i] -> HTML i
button xs = element (tagName "button") xs

button_ :: [HTML i] -> HTML i
button_ = button mempty

canvas :: [A.Attr i] -> [HTML i] -> HTML i
canvas xs = element (tagName "canvas") xs

canvas_ :: [HTML i] -> HTML i
canvas_ = canvas mempty

caption :: [A.Attr i] -> [HTML i] -> HTML i
caption xs = element (tagName "caption") xs

caption_ :: [HTML i] -> HTML i
caption_ = caption mempty

center :: [A.Attr i] -> [HTML i] -> HTML i
center xs = element (tagName "center") xs

center_ :: [HTML i] -> HTML i
center_ = center mempty

cite :: [A.Attr i] -> [HTML i] -> HTML i
cite xs = element (tagName "cite") xs

cite_ :: [HTML i] -> HTML i
cite_ = cite mempty

code :: [A.Attr i] -> [HTML i] -> HTML i
code xs = element (tagName "code") xs

code_ :: [HTML i] -> HTML i
code_ = code mempty

col :: [A.Attr i] -> [HTML i] -> HTML i
col xs = element (tagName "col") xs

col_ :: [HTML i] -> HTML i
col_ = col mempty

colgroup :: [A.Attr i] -> [HTML i] -> HTML i
colgroup xs = element (tagName "colgroup") xs

colgroup_ :: [HTML i] -> HTML i
colgroup_ = colgroup mempty

datalist :: [A.Attr i] -> [HTML i] -> HTML i
datalist xs = element (tagName "datalist") xs

datalist_ :: [HTML i] -> HTML i
datalist_ = datalist mempty

dd :: [A.Attr i] -> [HTML i] -> HTML i
dd xs = element (tagName "dd") xs

dd_ :: [HTML i] -> HTML i
dd_ = dd mempty

del :: [A.Attr i] -> [HTML i] -> HTML i
del xs = element (tagName "del") xs

del_ :: [HTML i] -> HTML i
del_ = del mempty

details :: [A.Attr i] -> [HTML i] -> HTML i
details xs = element (tagName "details") xs

details_ :: [HTML i] -> HTML i
details_ = details mempty

dfn :: [A.Attr i] -> [HTML i] -> HTML i
dfn xs = element (tagName "dfn") xs

dfn_ :: [HTML i] -> HTML i
dfn_ = dfn mempty

dialog :: [A.Attr i] -> [HTML i] -> HTML i
dialog xs = element (tagName "dialog") xs

dialog_ :: [HTML i] -> HTML i
dialog_ = dialog mempty

dir :: [A.Attr i] -> [HTML i] -> HTML i
dir xs = element (tagName "dir") xs

dir_ :: [HTML i] -> HTML i
dir_ = dir mempty

div :: [A.Attr i] -> [HTML i] -> HTML i
div xs = element (tagName "div") xs

div_ :: [HTML i] -> HTML i
div_ = div mempty

dl :: [A.Attr i] -> [HTML i] -> HTML i
dl xs = element (tagName "dl") xs

dl_ :: [HTML i] -> HTML i
dl_ = dl mempty

dt :: [A.Attr i] -> [HTML i] -> HTML i
dt xs = element (tagName "dt") xs

dt_ :: [HTML i] -> HTML i
dt_ = dt mempty

em :: [A.Attr i] -> [HTML i] -> HTML i
em = element (tagName "em")

em_ :: [HTML i] -> HTML i
em_ = em mempty

embed :: [A.Attr i] -> [HTML i] -> HTML i
embed xs = element (tagName "embed") xs

embed_ :: [HTML i] -> HTML i
embed_ = embed mempty

fieldset :: [A.Attr i] -> [HTML i] -> HTML i
fieldset xs = element (tagName "fieldset") xs

fieldset_ :: [HTML i] -> HTML i
fieldset_ = fieldset mempty

figcaption :: [A.Attr i] -> [HTML i] -> HTML i
figcaption xs = element (tagName "figcaption") xs

figcaption_ :: [HTML i] -> HTML i
figcaption_ = figcaption mempty

figure :: [A.Attr i] -> [HTML i] -> HTML i
figure xs = element (tagName "figure") xs

figure_ :: [HTML i] -> HTML i
figure_ = figure mempty

font :: [A.Attr i] -> [HTML i] -> HTML i
font xs = element (tagName "font") xs

font_ :: [HTML i] -> HTML i
font_ = font mempty

footer :: [A.Attr i] -> [HTML i] -> HTML i
footer xs = element (tagName "footer") xs

footer_ :: [HTML i] -> HTML i
footer_ = footer mempty

form :: [A.Attr i] -> [HTML i] -> HTML i
form xs = element (tagName "form") xs

form_ :: [HTML i] -> HTML i
form_ = form mempty

frame :: [A.Attr i] -> [HTML i] -> HTML i
frame xs = element (tagName "frame") xs

frame_ :: [HTML i] -> HTML i
frame_ = frame mempty

frameset :: [A.Attr i] -> [HTML i] -> HTML i
frameset xs = element (tagName "frameset") xs

frameset_ :: [HTML i] -> HTML i
frameset_ = frameset mempty

h1 :: [A.Attr i] -> [HTML i] -> HTML i
h1 xs = element (tagName "h1") xs

h1_ :: [HTML i] -> HTML i
h1_ = h1 mempty

h2 :: [A.Attr i] -> [HTML i] -> HTML i
h2 xs = element (tagName "h2") xs

h2_ :: [HTML i] -> HTML i
h2_ = h2 mempty

h3 :: [A.Attr i] -> [HTML i] -> HTML i
h3 xs = element (tagName "h3") xs

h3_ :: [HTML i] -> HTML i
h3_ = h3 mempty

h4 :: [A.Attr i] -> [HTML i] -> HTML i
h4 xs = element (tagName "h4") xs

h4_ :: [HTML i] -> HTML i
h4_ = h4 mempty

h5 :: [A.Attr i] -> [HTML i] -> HTML i
h5 xs = element (tagName "h5") xs

h5_ :: [HTML i] -> HTML i
h5_ = h5 mempty

h6 :: [A.Attr i] -> [HTML i] -> HTML i
h6 xs = element (tagName "h6") xs

h6_ :: [HTML i] -> HTML i
h6_ = h6 mempty

head :: [A.Attr i] -> [HTML i] -> HTML i
head xs = element (tagName "head") xs

head_ :: [HTML i] -> HTML i
head_ = head mempty

header :: [A.Attr i] -> [HTML i] -> HTML i
header xs = element (tagName "header") xs

header_ :: [HTML i] -> HTML i
header_ = header mempty

hr :: [A.Attr i] -> [HTML i] -> HTML i
hr xs = element (tagName "hr") xs

hr_ :: [HTML i] -> HTML i
hr_ = hr mempty

html :: [A.Attr i] -> [HTML i] -> HTML i
html xs = element (tagName "html") xs

html_ :: [HTML i] -> HTML i
html_ = html mempty

i :: [A.Attr i] -> [HTML i] -> HTML i
i xs = element (tagName "i") xs

i_ :: [HTML i] -> HTML i
i_ = i mempty

iframe :: [A.Attr i] -> [HTML i] -> HTML i
iframe xs = element (tagName "iframe") xs

iframe_ :: [HTML i] -> HTML i
iframe_ = iframe mempty

img :: [A.Attr i] -> [HTML i] -> HTML i
img xs = element (tagName "img") xs

img_ :: [HTML i] -> HTML i
img_ = img mempty

input :: [A.Attr i] -> [HTML i] -> HTML i
input xs = element (tagName "input") xs

input_ :: [HTML i] -> HTML i
input_ = input mempty

ins :: [A.Attr i] -> [HTML i] -> HTML i
ins xs = element (tagName "ins") xs

ins_ :: [HTML i] -> HTML i
ins_ = ins mempty

kbd :: [A.Attr i] -> [HTML i] -> HTML i
kbd xs = element (tagName "kbd") xs

kbd_ :: [HTML i] -> HTML i
kbd_ = kbd mempty

keygen :: [A.Attr i] -> [HTML i] -> HTML i
keygen xs = element (tagName "keygen") xs

keygen_ :: [HTML i] -> HTML i
keygen_ = keygen mempty

label :: [A.Attr i] -> [HTML i] -> HTML i
label xs = element (tagName "label") xs

label_ :: [HTML i] -> HTML i
label_ = label mempty

legend :: [A.Attr i] -> [HTML i] -> HTML i
legend xs = element (tagName "legend") xs

legend_ :: [HTML i] -> HTML i
legend_ = legend mempty

li :: [A.Attr i] -> [HTML i] -> HTML i
li xs = element (tagName "li") xs

li_ :: [HTML i] -> HTML i
li_ = li mempty

link :: [A.Attr i] -> [HTML i] -> HTML i
link xs = element (tagName "link") xs

link_ :: [HTML i] -> HTML i
link_ = link mempty

main :: [A.Attr i] -> [HTML i] -> HTML i
main xs = element (tagName "main") xs

main_ :: [HTML i] -> HTML i
main_ = main mempty

map :: [A.Attr i] -> [HTML i] -> HTML i
map xs = element (tagName "map") xs

map_ :: [HTML i] -> HTML i
map_ = map mempty

mark :: [A.Attr i] -> [HTML i] -> HTML i
mark xs = element (tagName "mark") xs

mark_ :: [HTML i] -> HTML i
mark_ = mark mempty

menu :: [A.Attr i] -> [HTML i] -> HTML i
menu xs = element (tagName "menu") xs

menu_ :: [HTML i] -> HTML i
menu_ = menu mempty

menuitem :: [A.Attr i] -> [HTML i] -> HTML i
menuitem xs = element (tagName "menuitem") xs

menuitem_ :: [HTML i] -> HTML i
menuitem_ = menuitem mempty

meta :: [A.Attr i] -> [HTML i] -> HTML i
meta xs = element (tagName "meta") xs

meta_ :: [HTML i] -> HTML i
meta_ = meta mempty

meter :: [A.Attr i] -> [HTML i] -> HTML i
meter xs = element (tagName "meter") xs

meter_ :: [HTML i] -> HTML i
meter_ = meter mempty

nav :: [A.Attr i] -> [HTML i] -> HTML i
nav xs = element (tagName "nav") xs

nav_ :: [HTML i] -> HTML i
nav_ = nav mempty

noframes :: [A.Attr i] -> [HTML i] -> HTML i
noframes xs = element (tagName "noframes") xs

noframes_ :: [HTML i] -> HTML i
noframes_ = noframes mempty

noscript :: [A.Attr i] -> [HTML i] -> HTML i
noscript xs = element (tagName "noscript") xs

noscript_ :: [HTML i] -> HTML i
noscript_ = noscript mempty

object :: [A.Attr i] -> [HTML i] -> HTML i
object xs = element (tagName "object") xs

object_ :: [HTML i] -> HTML i
object_ = object mempty

ol :: [A.Attr i] -> [HTML i] -> HTML i
ol xs = element (tagName "ol") xs

ol_ :: [HTML i] -> HTML i
ol_ = ol mempty

optgroup :: [A.Attr i] -> [HTML i] -> HTML i
optgroup xs = element (tagName "optgroup") xs

optgroup_ :: [HTML i] -> HTML i
optgroup_ = optgroup mempty

option :: [A.Attr i] -> [HTML i] -> HTML i
option xs = element (tagName "option") xs

option_ :: [HTML i] -> HTML i
option_ = option mempty

output :: [A.Attr i] -> [HTML i] -> HTML i
output xs = element (tagName "output") xs

output_ :: [HTML i] -> HTML i
output_ = output mempty

p :: [A.Attr i] -> [HTML i] -> HTML i
p xs = element (tagName "p") xs

p_ :: [HTML i] -> HTML i
p_ = p mempty

param :: [A.Attr i] -> [HTML i] -> HTML i
param xs = element (tagName "param") xs

param_ :: [HTML i] -> HTML i
param_ = param mempty

pre :: [A.Attr i] -> [HTML i] -> HTML i
pre xs = element (tagName "pre") xs

pre_ :: [HTML i] -> HTML i
pre_ = pre mempty

progress :: [A.Attr i] -> [HTML i] -> HTML i
progress xs = element (tagName "progress") xs

progress_ :: [HTML i] -> HTML i
progress_ = progress mempty

q :: [A.Attr i] -> [HTML i] -> HTML i
q xs = element (tagName "q") xs

q_ :: [HTML i] -> HTML i
q_ = q mempty

rp :: [A.Attr i] -> [HTML i] -> HTML i
rp xs = element (tagName "rp") xs

rp_ :: [HTML i] -> HTML i
rp_ = rp mempty

rt :: [A.Attr i] -> [HTML i] -> HTML i
rt xs = element (tagName "rt") xs

rt_ :: [HTML i] -> HTML i
rt_ = rt mempty

ruby :: [A.Attr i] -> [HTML i] -> HTML i
ruby xs = element (tagName "ruby") xs

ruby_ :: [HTML i] -> HTML i
ruby_ = ruby mempty

s :: [A.Attr i] -> [HTML i] -> HTML i
s xs = element (tagName "s") xs

s_ :: [HTML i] -> HTML i
s_ = s mempty

samp :: [A.Attr i] -> [HTML i] -> HTML i
samp xs = element (tagName "samp") xs

samp_ :: [HTML i] -> HTML i
samp_ = samp mempty

script :: [A.Attr i] -> [HTML i] -> HTML i
script xs = element (tagName "script") xs

script_ :: [HTML i] -> HTML i
script_ = script mempty

section :: [A.Attr i] -> [HTML i] -> HTML i
section xs = element (tagName "section") xs

section_ :: [HTML i] -> HTML i
section_ = section mempty

select :: [A.Attr i] -> [HTML i] -> HTML i
select xs = element (tagName "select") xs

select_ :: [HTML i] -> HTML i
select_ = select mempty

small :: [A.Attr i] -> [HTML i] -> HTML i
small xs = element (tagName "small") xs

small_ :: [HTML i] -> HTML i
small_ = small mempty

source :: [A.Attr i] -> [HTML i] -> HTML i
source xs = element (tagName "source") xs

source_ :: [HTML i] -> HTML i
source_ = source mempty

span :: [A.Attr i] -> [HTML i] -> HTML i
span xs = element (tagName "span") xs

span_ :: [HTML i] -> HTML i
span_ = span mempty

strike :: [A.Attr i] -> [HTML i] -> HTML i
strike xs = element (tagName "strike") xs

strike_ :: [HTML i] -> HTML i
strike_ = strike mempty

strong :: [A.Attr i] -> [HTML i] -> HTML i
strong xs = element (tagName "strong") xs

strong_ :: [HTML i] -> HTML i
strong_ = strong mempty

style :: [A.Attr i] -> [HTML i] -> HTML i
style xs = element (tagName "style") xs

style_ :: [HTML i] -> HTML i
style_ = style mempty

sub :: [A.Attr i] -> [HTML i] -> HTML i
sub xs = element (tagName "sub") xs

sub_ :: [HTML i] -> HTML i
sub_ = sub mempty

summary :: [A.Attr i] -> [HTML i] -> HTML i
summary xs = element (tagName "summary") xs

summary_ :: [HTML i] -> HTML i
summary_ = summary mempty

sup :: [A.Attr i] -> [HTML i] -> HTML i
sup xs = element (tagName "sup") xs

sup_ :: [HTML i] -> HTML i
sup_ = sup mempty

table :: [A.Attr i] -> [HTML i] -> HTML i
table xs = element (tagName "table") xs

table_ :: [HTML i] -> HTML i
table_ = table mempty

tbody :: [A.Attr i] -> [HTML i] -> HTML i
tbody xs = element (tagName "tbody") xs

tbody_ :: [HTML i] -> HTML i
tbody_ = tbody mempty

td :: [A.Attr i] -> [HTML i] -> HTML i
td xs = element (tagName "td") xs

td_ :: [HTML i] -> HTML i
td_ = td mempty

textarea :: [A.Attr i] -> [HTML i] -> HTML i
textarea xs = element (tagName "textarea") xs

textarea_ :: [HTML i] -> HTML i
textarea_ = textarea mempty

tfoot :: [A.Attr i] -> [HTML i] -> HTML i
tfoot xs = element (tagName "tfoot") xs

tfoot_ :: [HTML i] -> HTML i
tfoot_ = tfoot mempty

th :: [A.Attr i] -> [HTML i] -> HTML i
th xs = element (tagName "th") xs

th_ :: [HTML i] -> HTML i
th_ = th mempty

thead :: [A.Attr i] -> [HTML i] -> HTML i
thead xs = element (tagName "thead") xs

thead_ :: [HTML i] -> HTML i
thead_ = thead mempty

time :: [A.Attr i] -> [HTML i] -> HTML i
time xs = element (tagName "time") xs

time_ :: [HTML i] -> HTML i
time_ = time mempty

title :: [A.Attr i] -> [HTML i] -> HTML i
title xs = element (tagName "title") xs

title_ :: [HTML i] -> HTML i
title_ = title mempty

tr :: [A.Attr i] -> [HTML i] -> HTML i
tr xs = element (tagName "tr") xs

tr_ :: [HTML i] -> HTML i
tr_ = tr mempty

track :: [A.Attr i] -> [HTML i] -> HTML i
track xs = element (tagName "track") xs

track_ :: [HTML i] -> HTML i
track_ = track mempty

tt :: [A.Attr i] -> [HTML i] -> HTML i
tt xs = element (tagName "tt") xs

tt_ :: [HTML i] -> HTML i
tt_ = tt mempty

u :: [A.Attr i] -> [HTML i] -> HTML i
u xs = element (tagName "u") xs

u_ :: [HTML i] -> HTML i
u_ = u mempty

ul :: [A.Attr i] -> [HTML i] -> HTML i
ul xs = element (tagName "ul") xs

ul_ :: [HTML i] -> HTML i
ul_ = ul mempty

var :: [A.Attr i] -> [HTML i] -> HTML i
var xs = element (tagName "var") xs

var_ :: [HTML i] -> HTML i
var_ = var mempty

video :: [A.Attr i] -> [HTML i] -> HTML i
video xs = element (tagName "video") xs

video_ :: [HTML i] -> HTML i
video_ = video mempty

wbr :: [A.Attr i] -> [HTML i] -> HTML i
wbr xs = element (tagName "wbr") xs

wbr_ :: [HTML i] -> HTML i
wbr_ = wbr mempty
