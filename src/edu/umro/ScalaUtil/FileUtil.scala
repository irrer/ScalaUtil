package edu.umro.ScalaUtil

import java.io.File

object FileUtil {
    /**
     * Recursively compare two directories or files and throw an exception if their content is different.
     *
     * @param a
     *            One file or directory.
     *
     * @param b
     *            The other file or directory.
     * @return
     * @throws FileNotFoundException
     * @throws IOException
     */
    def compareDirs(a: File, b: File): Unit = {
        if (a.isDirectory && b.isDirectory) {
            val aList = a.listFiles
            val bList = b.listFiles
            if (aList.length != bList.length) throw new RuntimeException("Missing file")
            aList.map(aa => compareDirs(aa, new File(b, aa.getName())))
        }
        else {
            if (!(a.isFile() && b.isFile() && edu.umro.util.Utility.compareFiles(a, b))) throw new RuntimeException("files not equal");
        }
    }

    /**
     * Recursively compare two directories or files and throw an exception if
     * their content is different.  Any file access errors will result in a return of 'false';
     *
     * @param a
     *            One file or directory.
     *
     * @param b
     *            The other file or directory.
     *
     * @return True if the same, false if different.
     *
     * @throws FileNotFoundException
     * @throws IOException
     */
    def compareFolders(a: File, b: File): Boolean = {
        try {
            compareDirs(a, b);
            true
        }
        catch {
            case t: Throwable => false
        }
    }

    /**
     * Replace all characters in a name that are not supported by the Windows file system.
     * 
     * @name: File name
     * 
     * @replacement: Character to be used instead of original.
     * 
     * This will work for *nix systems too.  It will replace some characters that would be
     * allowed in *nix, but using them is generally not a good idea anyway.
     */
    def replaceInvalidFileNameCharacters(name: String, replacement: Char) = {
        val original = name.getBytes
        val modified = (0 until name.size).map(i => {
            val c = name.charAt(i)
            c match {
                case _ if (c == '\\') || (c == '/') || (c == ':') || (c == '*') || (c == '"') || (c == '<') || (c == '>') => replacement
                case _ if (c > 31) && (c < 127) => c
                case _ => replacement
            }
        }).toArray
        new String(modified)
    }

}