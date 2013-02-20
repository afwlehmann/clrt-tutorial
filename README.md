DISCLAIMER
==========

The source-code corresponds to [this series of screencasts](http://rudairandamacha.blogspot.de/2012/09/writing-simple-raytracer-in-common-lisp.html). As both the screencasts and the source-code were made in 2009, _all of this is only provided for convenience and totally unmaintained_.

LICENSE
=======

Copyright 2009 by Alexander Lehmann

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

DEPENDENCIES
============

The only dependency is [zpng](http://www.xach.com/lisp/zpng/).

In order to install and manage dependencies, this project makes use of both [asdf-install](http://www.cliki.net/ASDF-Install) and [asdf](http://common-lisp.net/project/asdf/).  Please note that the former has been deprecated for quite some time now (keep in mind that this source-code and the corresponding tutorial were made in 2009), however it still works ok in this case. Also note that both probably come pre-installed with your current Lisp implementation, e.g. [sbcl](http://www.sbcl.org/).

Using `asdf-install` makes installing the necessary dependencies fairly easy:

```lisp
(require 'asdf-install)
(asdf-install:install 'zpng)
```

In case you haven't used `asdf-install` before, don't get anxious when you see something like the following warning about a missing GPG key. Just skip the GPG check and go on with the installation process:

      No key found for key id 0x71CA4AFEE03213D2.  Try some command like 
      gpg  --recv-keys 0x71CA4AFEE03213D2
    
    Type HELP for debugger help, or (SB-EXT:QUIT) to exit from SBCL.
    
    restarts (invokable by number or by possibly-abbreviated name):
      0: [SKIP-GPG-CHECK] Don't check GPG signature for this package
      1: [ABORT         ] Exit debugger, returning to top level.

Another thing that might show up during installation is a warning about a missing component (most likely `zpng` or `salza2`). If you run into this, simply choose to retry finding the missing module after reinitializing the source-registry:

      Component "zpng" not found
    
    Type HELP for debugger help, or (SB-EXT:QUIT) to exit from SBCL.
    
    restarts (invokable by number or by possibly-abbreviated name):
      0: [REINITIALIZE-SOURCE-REGISTRY-AND-RETRY] Retry finding system zpng after
                                                  reinitializing the
                                                  source-registry.
      1: [RETRY                                 ] Retry installation
      2: [ABORT                                 ] Exit debugger, returning to top
                                                  level.

Now you're ready to run.

RUNNING THE EXAMPLE
===================

You can find the setup for a simple example scene in `simple-scene.lisp`. Running the example is easy once you've installed the necessary dependencies.

First change to the directory that contains the sources, then start you're favorite Lisp interpreter and off you go:

```lisp
(require 'asdf)
(asdf:load-system 'clrt)
(load "simple-scene")
(simple-scene:render)
```

The resulting image will then be saved as `test.png`.
