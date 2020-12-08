Secret Santa App
================

**On Hiatus**

This project has been temporarily suspended until next year.

The Secret Santa App is written in a combination of Haskell (assignment client
and web server) and Javascript (front-end).  This app manages a Secret Santa
campaign, in which people are randomly assigned a person to give a gift.

Users create an account with a name and e-mail address.  They then enter a
password to register.  For registration to be successful, they must use a name
and e-mail that matches a list provided when setting up.  As part of the
registration process, an elliptic curve keypair is generated.  The private key
is encrypted using a key derived from their password.

Two special admin users can log into a dashboard that displays the current
registration status of users.  Once all users have registered, these admin users
can run the client to pull down all public keys and generate the assignments.
These are encrypted using the public keys and uploaded back to the server.

Users can log in and retrieve their assignments.

Since it is possible that users may forget their passwords, one of these admin
users is an arbitrator.  If BOTH users request arbitration for a specific user,
their combined keys can be used to decrypt this user's assignment.  To keep
things fair, both keys have to be used.

To keep things festive, the admin's account is Santa, and the arbitrator's
account is the Elf.  During deployment, the client asks both Santa and the Elf
to select passwords.  Once passwords have been entered, a tarball will be
generated that can be uploaded to the server instance to install the server.
This tarball expects to find a config file in the project root, which is used to
read the TLS certificate and key file.

During gift assignment, both Santa and the Elf must again enter their passwords
into the client.  The list of registrations is downloaded, and if complete, then
gift assignment occurs and is uploaded.

If any of the users forget their passwords, both Santa and the Elf can perform
the arbitration action to recover this gift assignment.

Setup Configuration
-------------------

The Secret Santa App expects to find a file in the current working directory
called `users.txt`.  This file contains zero or more e-mail addresses in the
RFC-5322 format.  For example:

    John Smith <john.smith@example.com>

The descriptive part of this address should be the full name of the user as the
user would write it.  This is free-form.  The app will strip any white space
between each word in the name to create a canonical name.  Capitalization is
ignored.  The user must enter a name that matches this canonical name (e.g.
"John Smith", " John    Smith ", or "john smith").  The string between the angle
brackets is the e-mail address, which the user must type exactly.

A second file, called `preferences.txt` contains lines with an e-mail address
and the filename for a preferences file.  This file should be HTML, and will be
displayed to the assigned user as HTML.  For example:

    <john.smith@example.com>    john_smith_preferences.html

The path to the file name should resolve to something that the app can find.
This will be bundled into the deployment tarball.
