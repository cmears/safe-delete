Make sure that hard links do not confuse witnessing.
  (done)

Make sure that symbolic links do not confuse witnessing.
  (done)

Make sure symbolic links are not deleted.

Make sure symbolic links are not followed:
  * as a potential witness file
  * as a directory during the witness search
    (done)

Make sure that the target file cannot be used as its own witness,
especially when there are links around.
  (check inode?)
  (done)

Check that the files in matchingContents are actually loaded lazily
(that is, memory use is constant).
  (done)

Make sure matchingContents actually closes the files.
  (done)

Checking a directory against another could be much faster if the
metadata of the base directory was cached across targets.
