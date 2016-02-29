safe-delete: delete a file safely

Deletes a file, but only if another file with the same contents still
exists.  Can be used to remove duplicate files safely.

A file is deleted only if a "witness" for that file is found.  A
witness is another file with the same contents.  Symbolic links are
never followed, except for specifying the base directory.

Usage:

safe-delete [options] file ...

Options:

--base dir

  Specify the directory to look in for the witness file.
  Subdirectories are searched recursively.  By default, this is the
  current working directory.

--dry-run

  Don't actually delete anything, just report what would be
  deleted.
