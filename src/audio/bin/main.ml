(* file: sound.ml
  author: Bob Muller

  CSCI 1103 Computer Science 1 Honors

  This code develops a few applications of arrays in OCaml using WAV files.

  Depends on lightweight WAV API in:

    https://github.com/akabe/ocaml-numerical-analysis

  Compile and Run:
  > dune exec bin/main.exe

  Then open the generated wave files with Audacity.
*)

let sampleRate = 11025.0    (* Hz *)
let maxSample = 0.99999     (* -1.0 < amplitude < 1.0 *)
let frequencyOfA4 = 440.0   (* Hz *)

type sound = float array

(* Three functions for making a single wave of the appropriate wavelength.

  makeSinWave : int -> sound

  The call (makeSinWave waveLength) returns one sin wave with waveLength samples.
*)
let makeSinWave waveLength =
  failwith "makeSinWave ins't implemented"

let makeSawtoothWave waveLength =
  let sound = Array.make waveLength 0.0
  in
  for i = 0 to (waveLength - 1) do
    let sample = 2.0 *. (float i /. float waveLength)
    in
    sound.(i) <- sample -. 1.0
  done
  ; sound

let makeSharktoothWave waveLength =
  failwith "makeSharktoothWave ins't implemented"

let makeSquareWave waveLength =
  failwith "makeSquareWave ins't implemented"

(* copyWave : sound -> int -> sound

   The call (copyWave wave nSamples) makes a sound by copying wave.
*)
let copyWave wave nSamples =
  let waveLength = Array.length wave in
  let sound = Array.make nSamples 0.0
  in
  for i = 0 to (nSamples - 1) do
    sound.(i) <- wave.(i mod waveLength)
  done
  ; sound

(* frequencyOf : int -> float
*)
let frequencyOf halfSteps =
  let coefficient = 2.0 ** ((float halfSteps) /. 12.0)
  in
  coefficient *. frequencyOfA4

(* makeNote : float -> int -> (int -> sound) -> sound
*)
let makeNote seconds halfSteps waveMaker =
  let frequency  = frequencyOf halfSteps in
  let waveLength = int_of_float (sampleRate /. frequency) in
  let wave = waveMaker waveLength
  in
  copyWave wave (int_of_float (seconds *. sampleRate))

(* makeStereo : sound -> sound -> Lib.wav_data
*)
let makeStereo left right =
  let pair left right = (left, right)
  in
  Lib.STEREO (Array.map2 pair left right)

(*************** Using the tools above to make some WAV files *****************)

(* a4sin
*)
let a4sin () =
  let a4sin  = makeNote 3.0 0 makeSinWave in
  let stereo = makeStereo a4sin a4sin
  in
  Lib.save ~sampling_rate:(int_of_float sampleRate) "a4sin.wav" stereo

(* a4quare
*)
let a4square () =
  let a4shark  = makeNote 3.0 0 makeSquareWave in
  let stereo = makeStereo a4shark a4shark
  in
  Lib.save ~sampling_rate:(int_of_float sampleRate) "a4square.wav" stereo

(* a4saw
*)
let a4saw () =
  let a4shark  = makeNote 3.0 0 makeSawtoothWave in
  let stereo = makeStereo a4shark a4shark
  in
  Lib.save ~sampling_rate:(int_of_float sampleRate) "a4saw.wav" stereo

(* a4shark
*)
let a4shark () =
  let a4shark  = makeNote 3.0 0 makeSharktoothWave in
  let stereo = makeStereo a4shark a4shark
  in
  Lib.save ~sampling_rate:(int_of_float sampleRate) "a4shark.wav" stereo

(************************** Run the code *************************************

  go : unit -> unit

  The call (go ()) should create a set of four WAV files.
*)
let go () =
  (
    a4sin ()
  ; a4square()
  ; a4saw()
  ; a4shark ()
  )

let s = go ()
