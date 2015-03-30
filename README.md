Functional Lenses
=================

This package provides some basic types and functions for using lenses in OCaml.
Functional lenses are based on F# implementation in [FSharpX](https://github.com/fsharp/fsharpx). See [src/FSharpx.Core/Lens.fs](https://github.com/fsharp/fsharpx/blob/master/src/FSharpx.Core/Lens.fs) for the original implementation.  Written by Alessandro Strada.

See also:
* <http://bugsquash.blogspot.com/2011/11/lenses-in-f.html> Lenses in F#
* <http://stackoverflow.com/questions/8179485/updating-nested-immutable-data-structures> Stackoverflow question about Updating nested immutable data structures
* <http://stackoverflow.com/questions/5767129/lenses-fclabels-data-accessor-which-library-for-structure-access-and-mutatio> Haskell libraries for structure access and mutation
* <http://www.youtube.com/watch?v=efv0SQNde5Q> Functional lenses for Scala by Edward Kmett on YouTube
* <http://patternsinfp.wordpress.com/2011/01/31/lenses-are-the-coalgebras-for-the-costate-comonad/> Lenses are the coalgebras for the costate comonad by Jeremy Gibbons

Examples
========

First load `Lens` in utop.

    utop # #use "lens.ml";;

Given a couple of records

    type car = {
        make : string;
        model: string;
        mileage: int;
      };;

    type editor = {
        name: string;
        salary: int;
        car: car;
    };;

    type book = {
        name: string;
        author: string;
        editor: editor;
    };;

Create a new nested record

    let scifi_novel = {
       name =  "Metro 2033";
       author = "Dmitry Glukhovsky";
       editor =  {
         name = "Vitali Gubarev";
         salary =  1300;
         car =  {
           make = "Lada";
           model = "VAZ-2103";
           mileage = 310000
        }
      }
    };;

Now to construct a few lenses to access some things

    let car_lens = {
	    get = (fun x -> x.car);
	    set = (fun v x -> { x with car = v })
	  };;

    let editor_lens = {
	    get = (fun x -> x.editor);
	    set = (fun v x -> { x with editor = v })
	};;

    let mileage_lens = {
	    get = (fun x -> x.mileage);
	    set = (fun v x -> { x with mileage = v })

    };;

Using these lenses we can modify the mileage without having to unpack the record

    let a = compose mileage_lens (compose car_lens editor_lens) in
    _set 10 scifi_novel a;;

Or using the `Infix` module we can do the same thing, only shorter.

    _set 10 scifi_novel (editor_lens |-- car_lens |-- mileage_lens);;

    (* or *)

    ((editor_lens |-- car_lens |-- mileage_lens) ^= 10) @@ scifi_novel;;
