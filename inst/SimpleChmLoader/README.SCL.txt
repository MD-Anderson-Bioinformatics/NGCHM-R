Instructions for using the Simple CHM Loader (SCL) system

This note describes the protocol used by the SCL system.  The R library configuration
for SCL implements this protocol.  If you want to use it manually or from another
system, follow this protocol.

In what follows, the named subdirectories refer to the various subdirectories of the
'chmData' directory specified in the server configuration.

List the installed NG-CHMs:
    - SCL will periodically populate the "LIST" subdirectory with an empty directory for each
      installed NG-CHM.
 
Install an NG-CHM:
    - Create a working directory with the name of the NG-CHM concerned in the "STAGE" subdirectory.
    - Copy (cp)/unpack the NG-CHM concerned directly into this working directory (not a subdirectory thereof).
    - Once the copying is done, move (mv) this working directory into the "ADD" subdirectory.
    - When the CHM has been deployed by SCL, SCL will remove the directory from the "ADD" subdirectory.
    - If an old NGCHM with the same name is already deployed, the old NGCHM will be deleted.

Uninstall an NG-CHM:
    - Create a directory with the name of the NG-CHM concerned in the "REMOVE" subdirectory.
    - After the NG-CHM has been removed, SCL will remove the directory from the "REMOVE" subdirectory.

Add files to an NG-CHM:
    - Create a working directory with the name of the NG-CHM concerned in the "STAGE" subdirectory.
    - Add the files you want added to this working directory.
    - Once all files have been added, move (mv) the working directory into the "ADD-FILE" subdirectory.
    - After the files have been copied, SCL will remove the working directory from the "ADD-FILE" subdirectory.

Make an NG-CHM private:
    - Follow the instructions above to add a file "hidden.txt" to the NG-CHM concerned.

Remove files from an NG-CHM:
    - Create a working directory with the name of the NG-CHM concerned in the "STAGE" subdirectory.
    - Create a file in this working directory for each file you want removed from the NG-CHM.
    - Files can be zero length and created by touch; only the name matters.
    - Once the creation is done, move (mv) the working directory into the "REMOVE-FILE" subdirectory.
    - After the files have been removed, SCL will remove the working directory from the "REMOVE-FILE" subdirectory.

Make an NG-CHM public:
    - Follow the instructions above to remove the file "hidden.txt" from the NG-CHM concerned.


Acknowledgement:
The SCL protocol is based in part on the MDS protocol developed by Tod Casasent.
