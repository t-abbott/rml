open Utils

exception TypeError of string * Location.t
exception NameError of string * Location.t