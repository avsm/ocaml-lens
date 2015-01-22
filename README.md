Lens: Lenses
==================================

This package provides some basic types and functions for using lenses in OCaml.

Examples
==================================

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
