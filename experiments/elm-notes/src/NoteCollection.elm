module NoteCollection exposing (..)

import Note exposing (Note)


type alias NoteCollection =
    List Note


fromList =
    identity


all =
    identity


add content =
    (::) (Note.init content)


updateNoteContent content note =
    List.map
        (\n ->
            (if n == note then
                Note.setContent content n
             else
                n
            )
        )
