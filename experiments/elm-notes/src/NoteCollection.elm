module NoteCollection exposing (..)

import Note exposing (Note)
import Random


type alias NoteCollection =
    List Note


fromList =
    identity


all =
    identity


addNew content =
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


generator : Random.Generator NoteCollection
generator =
    Random.constant []
