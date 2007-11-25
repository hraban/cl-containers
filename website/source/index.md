{include resources/header.md}
{set-property title "CL-Containers"}

<div class="contents">
<div class="system-links">

  * [Mailing Lists][3]
  * [Getting it][4]
  * [Documentation][5]
  * [News][6]
  * [Test results][tr]
  * [Changelog][7]

   [3]: #mailing-lists
   [4]: #downloads
   [5]: documentation/ (documentation link)
   [6]: #news
   [7]: changelog.html
   [tr]: test-report.html

</div>
<div class="system-description">

### What it is

Common Lisp ships with a set of powerful built in data structures including the venerable list, full featured arrays, and hash-tables. CL-containers enhances and builds on these structures in two ways:

1. By adding containers that are not available in native Lisp (for example: binary search trees, red-black trees, sparse arrays and so on). 
2. By providing a standard interface so that they are simpler to use and so that changing design decisions becomes significantly easier.

Here is a slightly crazy picture of the class hierarchy for [containers][8] and for [generators. These are PDFs.][9]

   [8]: images/containers.pdf
   [9]: images/iterators.pdf

[TINAA][10] documentation for CL-Containers is [available][11].

   [10]: http://common-lisp.net/project/tinaa/
   [11]: http://common-lisp.net/project/cl-containers/documentation/

{anchor mailing-lists}

### Mailing Lists

  * [cl-containers-devel][13]: A list for questions, patches, bug reports, and so on; It's for everything other than announcements.

   [13]: http://common-lisp.net/cgi-bin/mailman/listinfo/cl-containers-devel

{anchor downloads}

### Where is it

A [Darcs][15] repository is available (note that you'll also need to get [Metatilities][] and [Moptilities][] to get CL-Containers to work). The darcs command is listed below:
    
    darcs get http://common-lisp.net/project/cl-containers

CL-Containers (and friends) should also be [ASDF installable][18]. Its CLiki home is right [where][19] you'd expect.

   [18]: http://www.cliki.net/asdf-install
   [19]: http://www.cliki.net/cl-containers

There's also a handy [gzipped tar file][20].

   [20]: http://common-lisp.net/project/cl-containers/cl-containers_latest.tar.gz

{anchor news}

### What is happening

24 October 2007 
Lots of little updates between then and now.

25 Nov 2005
Wrote a mini tutorial and published it on [unCLog][22].

   [22]: http://www.metabang.com/unclog/publisha/atinybit.html

14 Nov 2005
Added links, tarball, etc.

10 Nov 2005
Small patches and corrections; links to the mailing list. Nothing exciting

4 Nov 2005
OK. I changed my mind aboust waiting for ASDF. CL-Containers is ready for a release. Things are pretty weak-around-the-knees however, so the release party planned on Carnival Cruise Lines has been canceled. Sorry.

Today, I hope to add CLiki pages for CL-Containers and two other bits of code it relies on: Metatilities and Moptilities. I'll also finish with the darcs repositories, and have things ASDF installable. If I'm lucky, it'll actually work!

6 Oct 2005
I've decided to get cl-containers out without worrying about ASDF. This means I should be able to stick what's needed up on the web site by the end of this week (oh, oh, I've almost made a commitment). This includes: 

  * cl-containers
  * metatilities (everyone needs their own set of matching utilities)
  * moptilities (everyone needs their own MOP layer too)
  * generic-load-utilities

Most of this will be released using the MIT license although some of the code comes from long ago and far away and has it's own (quite unrestrictive) license. Once I've released, lots of good stuff will remain to do (asdf, testing, making sure it's platform compliant, etc). As always, stay tuned.

5 Oct 2005
I've got most of cl-containers under [darcs][23] now and split away from various unneccesary dependencies of the rest of my code. The next steps are:

   [23]: http://www.darcs.org/

  * Switching completely to asdf
  * Adding license information (cl-containers will be released under the MIT license)
  * Getting basic documentation ready
  * Making the source available

If things go moderately then "real soon now" may actually occur "real soon". Thanks for your patience.

29 July 2005
I'm in the process of converting cl-containers from a home grown defsystem to ASDF, of cleaning up some unnecessary dependencies and of trying to create a bit of documentation. I hope to have cl-containers up in some form real soon now. You can drop me e-mail if you're interested, would like to help or want to be notified when the code actually appears up here!

</div>
</div>

{include resources/footer.md}

   [25]: http://common-lisp.net/project/cl-containers/shared/buttons/xhtml.gif (valid xhtml button)
   [26]: http://validator.w3.org/check/referer (xhtml1.1)
   [27]: http://common-lisp.net/project/cl-containers/shared/buttons/hacker.png (hacker emblem)
   [28]: http://www.catb.org/hacker-emblem/ (hacker)
   [29]: http://common-lisp.net/project/cl-containers/shared/buttons/lml2-powered.png (lml2 powered)
   [30]: http://lml2.b9.com/ (lml2 powered)
   [31]: http://common-lisp.net/project/cl-containers/shared/buttons/lambda-lisp.png (ALU emblem)
   [32]: http://www.lisp.org/ (Association of Lisp Users)
   [33]: http://common-lisp.net/project/cl-containers/shared/buttons/lisp-lizard.png (Common-Lisp.net)
   [34]: http://common-lisp.net/ (Common-Lisp.net)


