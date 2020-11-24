(* file: tessellation.ml
 * author: Bob Muller
 * date: January 1, 2015
 *
 * Various simple tesselations of the plane.
 *
 * depends: Asai/Uehara Universe Library
 * compile: ./make
 * run: ./tessellation
*)
type bmp = Cairo.Image.data32

let rows bmp = Bigarray.Array2.dim1 bmp
let cols bmp = Bigarray.Array2.dim2 bmp

(* getPixel : bmp -> int * int -> Color.t *)
let getPixel bmp (row, col) =
  let rgba = bmp.{row, col} in
  let int255 = Int32.of_int 255 in
  let a = Int32.to_int (Int32.logand rgba int255) in
  let rgb = Int32.shift_right_logical rgba 8 in
  let b = Int32.to_int (Int32.logand rgb int255) in
  let rg = Int32.shift_right_logical rgb 8 in
  let g = Int32.to_int (Int32.logand rg int255) in
  let r = Int32.to_int (Int32.shift_right_logical rg 8)
  in
  Color.makeColor ~alpha:a r g b

(* setPixel : bmp -> int * int -> Color.t -> unit *)
let setPixel bmp (row, col) color =
  let colorToInt32 color =
    let (r, g, b, a) = Color.to_rgba color
    in
    Int32.of_int (((a * 256 + r) * 256 + g) * 256 + b)
  in
  bmp.{row, col} <- (colorToInt32 color)

let width  = 800.                (* window width *)
let height = 571.                (* window height *)

type state = Color | BlackAndWhite

(* toggle : state -> state *)
let toggle state =
  match state with
  | Color -> BlackAndWhite
  | BlackAndWhite -> Color

type model = { state : state
             ; image : Image.t
             }

let initialModel = { state = Color
                   ; image = Image.readImage (Sys.argv.(1))
                   }

(* view : model -> Image.t *)
let view model =
  match model.state with
  | Color -> model.image
  | BlackAndWhite ->
    let bmp = Image.toBitmap model.image
    in
    for row = 0 to rows bmp - 1 do
      for col = 0 to cols bmp - 1 do
        let c = getPixel bmp (row, col) in
        let (a, r, g, b) = Color.to_rgba c in
        let ave = int_of_float(((float r) +. (float g) +. (float b)) /. 3.0) in
        let newColor = Color.makeColor ~alpha:255 ave ave ave
        in
        setPixel bmp (row, col) newColor
      done
    done
    ; Image.fromBitmap bmp

(* handleTouchPad : model -> float -> float -> string -> model *)
let handleTouchPad model _ _ event =
  match event = "button_up" with
  | true -> { model with state = toggle model.state }
  | false -> model

(*********************************************************************)

(* animation start *)
let go () =
  Animate.start initialModel
    ~name:"Angela Merkel"
    ~view: view
    ~onMouse: handleTouchPad
    ~width:width
    ~height:height

let s = go ()
