#!/bin/bash

# Download using Subversion all OpenGL man pages XML data
SVNURL=https://cvs.khronos.org/svn/repos/ogl/trunk/ecosystem/public/sdk/docs/man4/
LOCALDIR=download
svn co --username anonymous --password anonymous $SVNURL $LOCALDIR
