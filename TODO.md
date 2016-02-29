Make sure that hard links do not confuse witnessing.

Make sure that symbolic links do not confuse witnessing.

Make sure symbolic links are not deleted.

Make sure symbolic links are not followed:
  * as a potential witness file
  * as a directory during the witness search

Make sure that the target file cannot be used as its own witness,
especially when there are links around.
  (check inode?)

Check that the files in matchingContents are actually loaded lazily
(that is, memory use is constant).

Make sure matchingContents actually closes the files.
