#r "node_modules/fable-core/Fable.Core.dll"
#load "html.fs"

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
  // | Horizontal

type Shape<[<Measure>] 'vx, [<Measure>] 'vy> = 
  | Area of (float<'vx> * float<'vy>)[]
  | Stack of Orientation * Shape<'vx, 'vy>[]

//type Scale = 
  //| Continuous of float * float
  // | Ordinal of string[]

let rec height (x:float<'vx>) (shape:Shape<'vx, 'vy>) : float<'vy> = 
  match shape with
  | Area(vals) -> 
      match vals |> Seq.tryFind (fun (vx, vy) -> vx = x) with
      | Some(_, y) -> y
      | _ -> failwith "TODO: Interpolation ?"
  | Stack(Vertical, shapes) -> shapes |> Seq.sumBy (height x)


/// Projection can turn a point ('vx * 'vy) to a point ('ux * 'uy)
type Projection<[<Measure>] 'vx, [<Measure>] 'vy, [<Measure>] 'ux, [<Measure>] 'uy> = 
  | Scale of (float<'vx> * float<'vy>) * (float<'vx> * float<'vy>) * (float<'ux> * float<'uy>) * (float<'ux> * float<'uy>)
  | Offset of Orientation * Shape<'vx, 'vy> * Projection<'vx, 'vy, 'ux, 'uy>
  | Center of Orientation * Shape<'vx, 'vy> * Projection<'vx, 'vy, 'ux, 'uy>

let rec project (x, y) = function
  | Scale((slx, sly), (shx, shy), (tlx, tly), (thx, thy)) ->
      printfn "%A --> %A" (x,y) ((slx, sly), (shx, shy), (tlx, tly), (thx, thy))
      printfn "(x - slx) / (shx - slx) * (thx - tlx) + tlx = %A" ((x - slx) / (shx - slx) * (thx - tlx) + tlx)
      (x - slx) / (shx - slx) * (thx - tlx) + tlx,
      (y - sly) / (shy - sly) * (thy - tly) + tly
  | Offset(Vertical, shape, proj) ->
      project (x, y + height x shape ) proj
  | Center(Vertical, shape, proj) ->
      project (x, y - (height x shape ) / 2.0<_>) proj

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

let usa = Area(medals |> Seq.item 0 |> snd)
let rus = Area(medals |> Seq.item 1 |> snd)
let all = Stack(Vertical, [| for _, c in medals -> Area(c) |])

type Svg =
  | Path of PathSegment[]
  | Combine of Svg * Svg
  | Empty

let drawArea p area = 
  let xs = area |> Array.map fst 
  let ys = area |> Array.map snd
  let x0, x1 = Array.min xs, Array.max xs
  let y0, y1 = Array.min ys, Array.max ys
  [ yield MoveTo(project (x0, y0) p) 
    for pt in area do yield LineTo (project pt p)
    for x,_ in Array.rev area do yield LineTo (project (x, 0.0<_>) p) ]
  |> Array.ofList |> Path

let rec drawShape p = function 
  | Area a -> drawArea p a 
  | Stack(Vertical, shapes) ->
      shapes 
      |> Array.fold (fun (p, svg) shape ->
          Offset(Vertical, shape, p), 
          Combine(svg, drawShape p shape)) (p, Empty)
      |> snd


  //| Stack(Vertical, shapes) -> 


//let master 
//let xscale, yscale = getScales shape

//let mapPoint (w, h) =
//  match xscale, yscale with
//  | Continuous(x0, x1), Continuous(y0, y1) -> fun (x, y) ->
//      (x - x0) / (x1 - x0) * w, (y - y0) / (y1 - y0) * h

let master = 
  Center(Vertical, all, Scale((1896.0<year>, 0.0<medal>), (2016.0<year>, 350.0<medal>), (0.0, 400.0), (1000.0, 1000.0)))
  
let svg = drawShape master all
  
open Fable.Core
open Fable.Import.Browser
open Fable.Html

let rnd = System.Random()
let randStyle() = sprintf "fill:rgb(%d,%d,%d);stroke-width:0" (rnd.Next(256)) (rnd.Next(256)) (rnd.Next(256))

let rec render svg = seq { 
  match svg with
  | Empty -> ()
  | Combine(s1, s2) ->
      yield! render s2
      yield! render s1
  | Path(p) ->
      yield s?path [ "d" => formatPath p; "style" => randStyle() ] [] }

s?svg ["width" => 1500; "height" => 1000] (List.ofSeq (render svg))  
|> renderTo (document.getElementById("container"))

