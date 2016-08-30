#r "node_modules/fable-core/Fable.Core.dll"
#load "html.fs"
open Fable.Import.Browser

type StringBuilder() = 
  let mutable strs = [] 
  member x.Append(s) = strs <- s::strs
  override x.ToString() = String.concat "" (List.rev strs)

type PathSegment = 
  | MoveTo of (float * float)
  | LineTo of (float * float)

let formatPath path = 
  let sb = StringBuilder()
  for ps in path do
    match ps with
    | MoveTo(x, y) -> sb.Append("M" + string x + " " + string y + " ")
    | LineTo(x, y) -> sb.Append("L" + string x + " " + string y + " ")
  sb.ToString()

let realign range def data = 
  let lookup = Map.ofSeq data
  [| for k in range -> k, defaultArg (lookup.TryFind k) def |]
  

type [<Measure>] year
type [<Measure>] medal

let rng = [ 1896.<year> .. 4.0<year> .. 2016.0<year> ]

let medals = 
  [ "USA",     realign rng 0.<medal> [(1896.<year>,12.<medal>);(1900.<year>,25.<medal>);(1904.<year>,89.<medal>);(1908.<year>,32.<medal>);(1912.<year>,39.<medal>);(1920.<year>,65.<medal>);(1924.<year>,67.<medal>);(1928.<year>,39.<medal>);(1932.<year>,69.<medal>);(1936.<year>,40.<medal>);(1948.<year>,58.<medal>);(1952.<year>,55.<medal>);(1956.<year>,53.<medal>);(1960.<year>,56.<medal>);(1964.<year>,66.<medal>);(1968.<year>,72.<medal>);(1972.<year>,71.<medal>);(1976.<year>,70.<medal>);(1984.<year>,131.<medal>);(1988.<year>,80.<medal>);(1992.<year>,97.<medal>);(1996.<year>,85.<medal>);(2000.<year>,79.<medal>);(2004.<year>,84.<medal>);(2008.<year>,91.<medal>);(2012.<year>,91.<medal>);(2016.<year>,54.<medal>)]
    "Russia",  realign rng 0.<medal> [(1908.<year>,3.<medal>);(1912.<year>,5.<medal>);(1952.<year>,53.<medal>);(1956.<year>,76.<medal>);(1960.<year>,81.<medal>);(1964.<year>,78.<medal>);(1968.<year>,80.<medal>);(1972.<year>,85.<medal>);(1976.<year>,101.<medal>);(1980.<year>,144.<medal>);(1988.<year>,107.<medal>);(1992.<year>,93.<medal>);(1996.<year>,59.<medal>);(2000.<year>,80.<medal>);(2004.<year>,77.<medal>);(2008.<year>,62.<medal>);(2012.<year>,68.<medal>);(2016.<year>,31.<medal>)]
    "UK",      realign rng 0.<medal> [(1896.<year>,7.<medal>);(1900.<year>,23.<medal>);(1904.<year>,2.<medal>);(1908.<year>,76.<medal>);(1912.<year>,37.<medal>);(1920.<year>,39.<medal>);(1924.<year>,29.<medal>);(1928.<year>,19.<medal>);(1932.<year>,16.<medal>);(1936.<year>,14.<medal>);(1948.<year>,23.<medal>);(1952.<year>,11.<medal>);(1956.<year>,21.<medal>);(1960.<year>,20.<medal>);(1964.<year>,18.<medal>);(1968.<year>,10.<medal>);(1972.<year>,17.<medal>);(1976.<year>,13.<medal>);(1980.<year>,19.<medal>);(1984.<year>,32.<medal>);(1988.<year>,22.<medal>);(1992.<year>,20.<medal>);(1996.<year>,15.<medal>);(2000.<year>,27.<medal>);(2004.<year>,29.<medal>);(2008.<year>,41.<medal>);(2012.<year>,57.<medal>);(2016.<year>,35.<medal>)]
    "Germany", realign rng 0.<medal> [(1896.<year>,11.<medal>);(1900.<year>,5.<medal>);(1904.<year>,9.<medal>);(1908.<year>,11.<medal>);(1912.<year>,18.<medal>);(1928.<year>,28.<medal>);(1932.<year>,19.<medal>);(1936.<year>,73.<medal>);(1952.<year>,22.<medal>);(1956.<year>,21.<medal>);(1960.<year>,41.<medal>);(1964.<year>,44.<medal>);(1968.<year>,45.<medal>);(1972.<year>,80.<medal>);(1976.<year>,95.<medal>);(1980.<year>,105.<medal>);(1984.<year>,54.<medal>);(1988.<year>,112.<medal>);(1992.<year>,74.<medal>);(1996.<year>,61.<medal>);(2000.<year>,52.<medal>);(2004.<year>,47.<medal>);(2008.<year>,37.<medal>);(2012.<year>,36.<medal>);(2016.<year>,17.<medal>)]
    "France",  realign rng 0.<medal> [(1896.<year>,8.<medal>);(1900.<year>,51.<medal>);(1908.<year>,14.<medal>);(1912.<year>,13.<medal>);(1920.<year>,36.<medal>);(1924.<year>,32.<medal>);(1928.<year>,19.<medal>);(1932.<year>,18.<medal>);(1936.<year>,18.<medal>);(1948.<year>,25.<medal>);(1952.<year>,16.<medal>);(1956.<year>,14.<medal>);(1960.<year>,5.<medal>);(1964.<year>,14.<medal>);(1968.<year>,13.<medal>);(1972.<year>,13.<medal>);(1976.<year>,9.<medal>);(1980.<year>,14.<medal>);(1984.<year>,26.<medal>);(1988.<year>,15.<medal>);(1992.<year>,28.<medal>);(1996.<year>,35.<medal>);(2000.<year>,37.<medal>);(2004.<year>,31.<medal>);(2008.<year>,38.<medal>);(2012.<year>,33.<medal>);(2016.<year>,25.<medal>)] ]

type Orientation = 
  | Vertical
  | Horizontal

type continuous<[<Measure>] 'u> = CO of float<'u> 
type categorical<[<Measure>] 'u> = CA of string
type value<[<Measure>] 'u> = 
  | CAV of categorical<'u> 
  | CAR of categorical<'u> * float
  | COV of continuous<'u>

type Shape<[<Measure>] 'vx, [<Measure>] 'vy> = 
  | Area of (continuous<'vx> * continuous<'vy>)[]
  | Bar of categorical<'vx> * continuous<'vy>
  | Stack of Orientation * Shape<'vx, 'vy>[]
  //| Space of float 
  | Padding of float * float * float * float * Shape<'vx, 'vy>
  | Center of Orientation * Shape<'vx, 'vy>

type Scale<[<Measure>] 'v> =
  | Continuous of continuous<'v> * continuous<'v>
  | Categorical of categorical<'v>[]

let rec height (shape:Shape<'vx, 'vy>) (x:value<'vx>) : continuous<'vy> = 
  match shape, x with
  | Area(vals), COV(x) -> 
      match vals |> Seq.tryFind (fun (vx, vy) -> vx = x) with
      | Some(_, y) -> y
      | _ -> failwith "TODO: Interpolation ?"
  | Bar(vx, vy), CAV(x) ->
      if x = vx then vy else CO 0.0<_>
  | Bar(vx, vy), CAR(x, f) ->
      if x = vx then vy else CO 0.0<_>
  | Stack(Vertical, shapes), x -> 
      shapes |> Seq.sumBy (fun s -> let (CO v) = height s x in v) |> CO
  | Stack(Horizontal, shapes), x -> 
      shapes |> Seq.map (fun s -> height s x) |> Seq.max
  | Padding(_, _, _, _, s), x ->
      height s x
  //| Space _, _ ->   
    //  CO 0.0<_>
  | x -> 
      Fable.Import.Browser.console.log("Height: %O", x)
      failwith "Cannot calculate height - mismatched X value"

let rec width (shape:Shape<'vx, 'vy>) (y:value<'vy>) : continuous<'vx> = 
  match shape, y with
  | Area(vals), COV(y) -> 
      match vals |> Seq.tryFind (fun (vx, vy) -> vy = y) with
      | Some(x, _) -> x
      | _ -> failwith "TODO: Interpolation ?"
  | Bar(vx, vy), _ ->
      CO (unbox<float<_>> 1.0) // Width of single Bar is 1.0
  | Stack(Horizontal, shapes), y -> 
      shapes |> Seq.sumBy (fun s -> let (CO v) = width s y in v) |> CO
  | Padding(_, _, _, _, s), y ->
      width s y
  //| Space _, _ ->
    //  CO 0.0<_>

  | _ -> failwith "Cannot calculate height - mismatched X value"

type Projection<[<Measure>] 'vx, [<Measure>] 'vy, [<Measure>] 'ux, [<Measure>] 'uy> = 
  | Scale of (float<'ux> * float<'ux>) * (float<'uy> * float<'uy>)
  | VerticalSpace of (value<'vx> -> continuous<'vy>) * Projection<'vx, 'vy, 'ux, 'uy>
  | HorizontalSpace of (value<'vy> -> continuous<'vx>) * Projection<'vx, 'vy, 'ux, 'uy>
  | VerticalOffset of top:float * bottom:float * Projection<'vx, 'vy, 'ux, 'uy>
  | HorizontalOffset of left:float * right:float * Projection<'vx, 'vy, 'ux, 'uy>
  | VerticalCenter of (value<'vx> -> continuous<'vy>) * Projection<'vx, 'vy, 'ux, 'uy>
  | HorizontalCenter of (value<'vy> -> continuous<'vx>) * Projection<'vx, 'vy, 'ux, 'uy>

let scaleOne (tlv:float<_>, thv:float<_>) scale coord = 
  match scale, coord with
  | Continuous(CO slv, CO shv), (COV (CO v)) ->
      COV(CO((v - slv) / (shv - slv) * (thv - tlv) + tlv))  
  | Categorical(vals), (CAR (CA v, f)) ->
      let size = (thv - tlv) / float vals.Length
      let i = vals |> Array.findIndex (fun (CA vv) -> v = vv)
      let i = float i + f
      COV(CO(tlv + (i * size)))
  | Categorical(vals), (CAV (CA v)) ->
      failwith "scaleOne: CAV in place of CAR"

let rec project<[<Measure>] 'vx, [<Measure>] 'vy, [<Measure>] 'ux, [<Measure>] 'uy> 
    (sx:Scale<'vx>) (sy:Scale<'vy>) point (projection:Projection<'vx, 'vy, 'ux, 'uy>) : value<'ux> * value<'uy> = 
  match projection, point with
  | Scale(tx, ty), (x, y) ->
      scaleOne tx sx x, scaleOne ty sy y 
  | VerticalSpace(height, p), (x, y) ->
      match y, height x with
      | COV (CO y), CO h -> project sx sy (x, COV (CO (y + h))) p
      | _ -> failwith "Cannot add vertical space to categorical values"
  | HorizontalSpace(width, p), (x, y) ->
      match x, width y with
      | COV (CO x), CO w -> project sx sy (COV (CO (x + w)), y) p
      | _ -> failwith "Cannot add horizontal space to categorical values"
  | VerticalOffset(top, bot, p), (x, y) ->
      match y, sy with
      | CAR((CA y), f), _ -> 
          let f = bot + unbox (f * (1.0 - (top + bot)))
          project sx sy (x, CAR((CA y), f)) p
      | COV(CO y), Continuous(CO lo, CO hi) -> 
          let innerSize = (hi - lo) * (1.0 - (top + bot))
          let k = (y - lo) / (hi - lo)
          let y = lo + (hi - lo) * bot + innerSize * k
          project sx sy (x, COV(CO y)) p
  | HorizontalOffset(left, right, p), (x, y) ->
      match x with
      | CAR((CA x), f) -> 
          let f = left + (f * (1.0 - (left + right)))
          project sx sy (CAR((CA x), f), y) p
      | _ -> 
          failwith "Cannot add horizontal offset around continuous value (TODO, probably rasonable)"  

  | VerticalCenter(height, p), (x, y) ->
      match sy, y, height x with
      | Continuous(CO lo, CO hi), COV(CO y), CO h -> 
          let offs = ((hi - lo) - h) / 2.0
          project sx sy (x, COV(CO(y + offs))) p
      | _ -> failwith "Cannot center categorical value"
  | HorizontalCenter(width, p), (x, y) ->
      match sx, x, width y with
      | Continuous(CO lo, CO hi), COV(CO x), CO w -> 
          let offs = ((hi - lo) - w) / 2.0
          project sx sy (COV(CO(x + offs)), y) p
      | _ -> failwith "Cannot center categorical value"

(*
let rec getScales = function
  | Stack(Vertical, shapes) ->
      let scales = shapes |> Array.map getScales
      let scalesx = scales |> Array.map fst
      let scalesy = scales |> Array.map fst
      scalesx |> Seq.reduce (fun (Continuous(l1, h1)) (Continuous(l2, h2)) -> Continuous(min l1 l2, max h1 h2)),
      scalesy |> Seq.fold (fun (Continuous(l1, h1)) (Continuous(l2, h2)) -> Continuous(l1, h1 + (h2 - l2))) (Continuous(0.0, 0.0))

  | Area(vals) ->
      let xs = vals |> Array.map fst 
      let ys = vals |> Array.map snd
      Continuous(Array.min xs, Array.max xs),
      Continuous(Array.min ys, Array.max ys)
*)

let unionScales s1 s2 =
  match s1, s2 with
  | Continuous(l1, h1), Continuous(l2, h2) -> Continuous(min l1 l2, max h1 h2)
  | Categorical(v1), Categorical(v2) -> Categorical(Array.distinct (Array.append v1 v2))
  | _ -> failwith "Cannot union continuous with categorical"

let rec getRanges shape = 
  match shape with
  | Area(a) -> let xr, yr = Array.unzip a in Array.map COV xr, Array.map COV yr
  | Bar(x, y) -> [| CAV x |], [| COV y |]
  | Stack(_, shapes) -> 
      let xr, yr = Array.map getRanges shapes |> Array.unzip
      Array.concat xr, Array.concat yr
  | Padding(l, r, t, b, shape) ->
      let xr, yr = getRanges shape
      // TODO: Probably should only allow padding on categorical?
      xr, yr

type Svg =
  | Path of PathSegment[]
  | Combine of Svg * Svg
  | Empty

let drawArea area = 
  let xs = area |> Array.map fst 
  let ys = area |> Array.map snd
  let x0, x1 = Array.min xs, Array.max xs
  let y0, y1 = Array.min ys, Array.max ys
  let sx, sy = Continuous(x0, x1), Continuous(y0, y1)
  (sx, sy), fun p (sx, sy) ->
    let projectCont (x, y) p = 
      match project sx sy (COV x, COV y) p with
      | COV (CO x), COV (CO y) -> x, y
      | _ -> failwith "Area can only be continuous"
    let path = 
      [ yield MoveTo(projectCont (x0, y0) p) 
        for pt in area do yield LineTo (projectCont pt p)
        for x,_ in Array.rev area do yield LineTo (projectCont (x, CO 0.0<_>) p) ]
      |> Array.ofList
    Path(path)

type IScaleSelector<[<Measure>] 'vx, [<Measure>] 'vy, [<Measure>] 'm, [<Measure>] 'o> = 
  abstract GetMainScales : Scale<'vx>[] * Scale<'vy>[] -> Scale<'m>[]
  abstract GetOtherScales : Scale<'vx>[] * Scale<'vy>[] -> Scale<'o>[]
  abstract GetOtherRanges : value<'vx>[] * value<'vy>[] -> value<'o>[]
  abstract MakeScalePair : Scale<'m> -> Scale<'o> -> Scale<'vx> * Scale<'vy>
  abstract MainSize : value<'o> -> continuous<'m>
  abstract MakeOffset : Shape<'vx, 'vy> -> Projection<'vx, 'vy, 1, 1> -> Projection<'vx, 'vy, 1, 1>

let xScaleSelector (shape:Shape<'vx, 'vy>) = 
  { new IScaleSelector<'vx, 'vy, 'vx, 'vy> with
      member x.GetMainScales(a, b) = a
      member x.GetOtherScales(a, b) = b
      member x.GetOtherRanges(a, b) = b
      member x.MakeScalePair a b = (a, b) 
      member x.MainSize l = width shape l
      member x.MakeOffset s p = HorizontalSpace(width s, p) }

let yScaleSelector (shape:Shape<'vx, 'vy>) = 
  { new IScaleSelector<'vx, 'vy, 'vy, 'vx> with
      member x.GetMainScales(a, b) = b
      member x.GetOtherScales(a, b) = a
      member x.GetOtherRanges(a, b) = a
      member x.MakeScalePair a b = (b, a) 
      member x.MainSize l = height shape l 
      member x.MakeOffset s p = VerticalSpace(height s, p) }

let stackShapes<[<Measure>] 'vx, [<Measure>] 'vy, [<Measure>] 'm, [<Measure>] 'o> 
      drawShape (scaleSel:IScaleSelector<'vx, 'vy, 'm, 'o>) (shape:Shape<'vx, 'vy>) shapes = 
  let ss, sfs = shapes |> Array.map drawShape |> Array.unzip
  let sxs, sys = ss |> Array.unzip

  // Other scale (non-stacked) scale is obtained by unioning individual scales
  let sother = scaleSel.GetOtherScales (sxs, sys) |> Array.reduce unionScales

  // Get main scale of sub-shapes & figure out if it is categorical or continuous
  let smain = scaleSel.GetMainScales (sxs, sys) 
  let scat = smain |> Array.choose (function Categorical(vs) -> Some vs | _ -> None)
  let scont = smain |> Array.choose (function Continuous(l, h) -> Some (l, h) | _ -> None)
  
  if scat.Length > 0 then
    // The main scale is union of categorical values from all stacked shapes
    let smain = scat |> Array.collect id |> Array.distinct |> Categorical
    scaleSel.MakeScalePair smain sother, fun p scales ->
      Array.zip shapes sfs |> Array.fold (fun svg (shape, f) ->
        Combine(svg, f p scales)) Empty

  elif scont.Length > 0 then
    // The main scale is calculated by looking at possible other-scale values
    // and finding maximal height of the main scale over all known points
    let rother = getRanges shape |> scaleSel.GetOtherRanges
    let smain = 
      let hmain = rother |> Array.map scaleSel.MainSize
      let hmin = Array.min hmain 
      let hmin = if hmin < CO 0.0<_> then hmin else CO 0.0<_>
      Continuous(hmin, Array.max hmain)

    scaleSel.MakeScalePair smain sother, fun p (scales:Scale<'vx> * Scale<'vy>) ->
      Array.zip shapes sfs |> Array.fold (fun (p, svg) (shape, f) ->
        scaleSel.MakeOffset shape p, 
          Combine(svg, f p scales)) (p, Empty) |> snd
  else
    // Mix of categorical and continuous scales cannot be stacked
    failwith "Stacked shapes must have matching kinds of scales"

          
let rec drawShape<[<Measure>] 'ux, [<Measure>] 'uy> (shape:Shape<'ux, 'uy>) = 
  match shape with
  | Area a -> drawArea a 
      
  | Padding(l, r, t, b, shape) ->
      let (sx, sy), f = drawShape shape
      (sx, sy), fun p (sx, sy) -> 
          let p = if l = 0.0 && r = 0.0 then p else HorizontalOffset(l, r, p)
          let p = if t = 0.0 && b = 0.0 then p else VerticalOffset(t, b, p)
          f p (sx, sy)

  | Bar(x, y) ->
      let sx, sy = Categorical [| x |], Continuous(y, y)
      (sx, sy), fun p (sx, sy) ->
        let x0, y0 = project sx sy (CAR (x, 0.0), COV (CO 0.0<_>)) p
        let x1, y1 = project sx sy (CAR (x, 1.0), COV y) p
        match x0, y0, x1, y1 with
        | COV (CO x0), COV (CO y0), COV (CO x1), COV (CO y1) ->
            [| MoveTo(x0, y0); LineTo(x0, y1)
               LineTo(x1, y1); LineTo(x1, y0); LineTo(x0, y0)  |] |> Path
        | _ -> failwith "Cannot draw categorical"

  | Stack(Horizontal, shapes) ->
      stackShapes drawShape (xScaleSelector shape) shape shapes
      
  | Stack(Vertical, shapes) ->
      stackShapes drawShape (yScaleSelector shape) shape shapes      

  | Center(orientation, shape) ->
      let scales, f = drawShape shape
      scales, fun p scales -> 
        let p = if orientation = Vertical then VerticalCenter(height shape, p) else HorizontalCenter(width shape, p)
        f p scales

let bar data : Shape<year, medal> = 
    let bars = data |> snd |> Array.map (fun (y, v) -> 
        Padding(0.1, 0.1, 0.0, 0.0, Bar(CA (string y), CO v)))
    Stack(Horizontal, bars)

let cont2 = Array.map (fun (y, v) -> CO y, CO v)
let usa = Area(medals |> Seq.item 0 |> snd |> cont2)
let rus = Area(medals |> Seq.item 1 |> snd |> cont2)
let all = Stack(Vertical, [| for _, c in medals -> Area(cont2 c) |])

let pad s = s // Padding(0.0, 0.0, 0.1, 0.1, s)
let barUsa = medals |> Seq.item 0 |> bar |> pad
let barRus = medals |> Seq.item 1 |> bar |> pad
let barRusUsa = Stack(Vertical, [| barUsa; barRus |])

let master = 
  Scale((0.0, 1500.0), (1000.0, 0.0))
  
let scales, f = drawShape (Center(Vertical, all))
let svg = f master scales
  
open Fable.Core
open Fable.Import.Browser
open Fable.Html

let rnd = System.Random()
let randStyle() = 
  let r, g, b = rnd.Next(256), rnd.Next(256), rnd.Next(256)
  sprintf "fill:rgb(%d,%d,%d); stroke:rgb(%d,%d,%d); stroke-width:1" r g b r g b

let rec render svg = seq { 
  match svg with
  | Empty -> ()
  | Combine(s1, s2) ->
      yield! render s2
      yield! render s1
  | Path(p) ->
      yield s?path [ "d" => formatPath p; "style" => randStyle() ] [] }

s?svg ["width" => 1500; "height" => 1000; "style" => "background:white"] (List.ofSeq (render svg))  
|> renderTo (document.getElementById("container"))


