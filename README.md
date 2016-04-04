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

--fast-match

  When checking for a witness, only look at the filename and the file
  size to determine a match.  The filename has all directory parts
  stripped; i.e. only part after the last slash.

--index

  Read the base directory in full and construct an index before
  searching for any target files.  This should make multiple-target
  searches much faster, especially in conjunction with --fast-match.


Examples:

Is it safe to delete file "bigfile"?  That is, does a copy of it
exist somewhere in the "backup" directory?

  safe-delete bigfile --base backup

As above, but run from the backup directory

  cd backup ; safe-delete ../bigfile


Does every file in directory "dir1" have a match in "dir2"?  Use fast
matching so that just filenames and sizes are checked.

  safe-delete dir1 --base dir2 --fast-match


Do these two directories have the same contents, except possibly for
directory arrangement?

  # Everything in dir1 exists in dir2.
  safe-delete dir1 --base dir2 --fast-match
  # Everything in dir2 exists in dir1.
  safe-delete dir2 --base dir1 --fast-match
  # And the contents are actually the same.
  safe-delete dir1 --base dir2
