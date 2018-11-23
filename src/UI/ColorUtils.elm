module UI.ColorUtils exposing(..)

import Element exposing(..)

type alias HSLA = {h: Float, s: Float, l: Float, a: Float}

toHsla : Color -> HSLA
toHsla color =
  let 
    {red, green, blue, alpha} = toRgb color
    (min, max) = case List.sort [red, green, blue] of
      a :: _ :: b :: [] -> (a, b)
      _ -> (0.0, 0.0) -- Shouldn't happen
    l = (min + max) / 2.0
    s = if min == max then 0.0
        else (max - min) / ( 1.0 - (abs (2*l-1.0)) )
    h6 = if red == green && green == blue then 0.0
         else if red >= green && red >= blue then (green-blue)/(max-min)
              else if green >= red && green >= blue then 2.0 + (blue-red)/(max-min)
                   else 4.0 + (red-green)/(max-min)
    h = if h6 < 0.0 then h6 / 6.0 + 1.0
                    else h6 / 6.0
  in {h=h, s=s, l=l, a=alpha}


fromHsla : HSLA -> Color
fromHsla {h, s, l, a} =
  if s == 0.0 then
    rgba l l l a
  else
    let 
      temp1 = if l < 0.5 then l+l*s
                         else l+s-l*s
      temp2 = 2.0*l-temp1
      tempr = if h < 2.0/3.0 then h + 1.0/3.0 else h - 2.0/3.0
      tempg = h
      tempb = if h >= 1.0/3.0 then h - 1.0/3.0 else h + 2.0/3.0
      norm tempx = if 6.0*tempx < 1.0 then temp2+(temp1-temp2)*6.0*tempx
                   else if 2.0*tempx < 1.0 then temp1
                        else if 3.0*tempx < 2.0 then temp2+(temp1-temp2)*(4.0-6.0*tempx)
                             else temp2
      r = max (norm tempr) 0.0
      g = max (norm tempg) 0.0
      b = max (norm tempb) 0.0
    in
      rgba r g b a


scaleAlpha : Float -> Color -> Color
scaleAlpha by color =
  let {red, green, blue, alpha} = toRgb color
  in rgba red green blue (alpha * by)

desaturate : Float -> Color -> Color
desaturate by color =
  let {red, green, blue, alpha} = toRgb color
  in rgba (red*(1.0-by)+by) (green*(1.0-by)+by) (blue*(1.0-by)+by) alpha

darken : Float -> Color -> Color
darken by color =
  let {red, green, blue, alpha} = toRgb color
  in rgba (red*(1.0-by)) (green*(1.0-by)) (blue*(1.0-by)) alpha

normalize : Float -> Color -> Color
normalize to color =
  let 
    {red, green, blue, alpha} = toRgb color
    value = sqrt(red*red + 1.5*green*green + blue*blue)
    scale = to / value
  in
    if isInfinite scale then rgba (to/sqrt(3.0)) (to/sqrt(3.0)) (to/sqrt(3.0)) alpha
    else rgba (red*scale) (green*scale) (blue*scale) alpha
