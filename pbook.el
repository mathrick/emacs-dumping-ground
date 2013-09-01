;;; pbook.el -- Format a program listing for LaTeX.
;;;
;;; More mangling by Paul Khuong on 2007-Jan-31 to typeset
;;;  code in a more generic and customisable manner, including
;;;  a rough xref (annotates definition sites).
;;; Mangled by Paul Khuong (pvk@pvk.ca) on 2007-Jan-20 to
;;;  emit coloured/bold/italic \tt code instead of verbatim
;;;      pbook.el,v 1.4 2007/01/20 
;;; Written by Luke Gorrie <luke@member.fsf.org> in May of 2004.
;;; $Id: pbook.el,v 1.3 2004/05/17 01:09:01 luke Exp luke $
;;;
;;; TODO:
;;;
;;; X Remove FIXMEs, etc from index.
;;;
;;; X Replace pbook-latex-escape with pbook-escape-code-substring
;;;    Find way to make space work.
;;;
;;; o Better xreferencing. Run a first pass to identify all 
;;;   the toplevel definitions [as fontified], then annotate
;;;   the source correctly; either change the face, or use the
;;;   escaping mechanism. Changing the face sounds more robust.
;;;   This could run as advice over pbook-process-buffer.
;;;   The face changing could be an advice on pbook-escape-code,
;;;   or, since we ignore face info outside of code, over the
;;;   whole buffer, w/ the definition identification.
;;;   If want to avoid multiple passes (why?), can accumulate
;;;   toplevel defn names while processing them.
;;;
;;; X Add a public variable for the current line number (for
;;;   property -> latex, especially)
;;;
;;; _ Make all/most of the extras togglable.
;;;
;;; X Rewrite the code escaping functions to use regexes instead of
;;;   searching ourselves.
;;;
;;; ? Allow escaped `raw' latex strings in comments
;;;
;;; o Improve (think) the interface for customising code output
;;;
;;; X Find some way to customise font-lock for paper automatically,
;;;   or introduce alists for italic, bold & colour override (per face)?
;;;   Currently: converts to yuv, flips the luminance and back to rgb.
;;;
;;;# Introduction
;;;
;;; Have you ever printed out a program and read it on paper?
;;;
;;; It is an interesting exercise to try with one of your own
;;; programs, one that you think is well-written. The first few times
;;; you will probably find that it's torture to try and read in a
;;; straight line. What seemed so nice in Emacs is riddled with
;;; glaring problems on paper.
;;;
;;; How a program reads on paper may not be very important in itself,
;;; but there is wonderful upside to this. If you go through the
;;; program with a red pen and fix all the mind-bendingly obvious
;;; problems you see, what happens is that the program greatly
;;; improves -- not just on paper, but also in Emacs!
;;;
;;; This is a marvellously effective way to make programs
;;; better.
;;;
;;; Let's explore the idea some more!
;;;
;;;# `pbook'
;;;
;;; This program, `pbook', is a tool for making readable programs by
;;; generating LaTeX'ified program listings. Its purpose is to help
;;; you improve your programs by making them read well on paper. It
;;; serves this end by generating pretty-looking PDF output for you to
;;; print out and attack with a red pen, and perhaps use the medium to
;;; trick your mind into seeking the clarity of a technical paper and
;;; bringing your prose-editing skills to bear on your source code.
;;;
;;; `pbook' is aware of three things: headings, top-level comments,
;;; and code. Headings become LaTeX sections, and have entries in a
;;; table of contents. Top-level comments become plain text in a nice
;;; variable-width font. Other source code is listed as-is in a
;;; fixed-width font.
;;;
;;; These different elements are distinguished in the source using
;;; maximally unobtrusive markup, which you can see at work in the
;;; `pbook.el' source code.
;;;
;;; Read on to see the program and how it works.
;;;
;;;# Prelude
;;;
;;; (I have successfully tested this program with GNU Emacs versions
;;; 20.7 and 21.3, and with XEmacs version 21.5.)
;;;
;;; This is actually not true anymore. I have tested this in GNU Emacs
;;; 22.??? and 21.???. While I don't expect there to be any portability
;;; problem, it has not been tested in XEmacs at all.
;;;
;;; For some tiny luxuries and portability help we use the Common Lisp
;;; compatibility library:
(require 'cl)

;;;# Emacs commands
;;;
;;; A handful of Emacs commands make up the pbook user-interface. The
;;; most fundamental is to render a pbook-formatted Emacs buffer as
;;; LaTeX.

(defun pbook-buffer ()
  "Generate LaTeX from the current (pbook-formatted) buffer.
The resulting source is displayed in a buffer called *pbook*."
  (interactive)
  (pbook-process-buffer))

;;; A very handy utility is to display a summary of the buffer's
;;; structure and use it to jump to an appropriate section. I've
;;; always enjoyed being able to do this in texinfo-mode. Happily,
;;; pbook gets this for free using the `occur' function, which lists
;;; all lines in the buffer that match some regular expression.

(defun pbook-show-structure ()
  "Display the pbook heading structure of the current buffer."
  (interactive)
  (occur pbook-heading-regexp))

;;; To avoid a lot of mucking about in the shell there is also a
;;; command to generate and display a PDF file. This function is a
;;; quick hack to make experimentation easy.
;;;
;;; I should add a function to do the same with dvis, and to output
;;; to non-temporary files.

(defun pbook-buffer-view-pdf ()
  "Generate and display PDF from the current buffer.
The intermediate files are created in the standard temporary
directory."
  (interactive)
  (save-window-excursion
    (pbook-buffer))
  (with-current-buffer "*pbook*"
    (let ((texfile (pbook-tmpfile "pbook" "tex"))
          (pdffile (pbook-tmpfile "pbook" "pdf"))
	  (idxfile (pbook-tmpfile "pbook" "idx")))
      (write-region (point-min) (point-max) texfile)
      ;; Possibly there is a better way to ensure that LaTeX generates
      ;; the table of contents correctly than to run it more than
      ;; once, but I don't know one.
      (shell-command (format "\
  cd /tmp; latex %s && \
  makeindex %s && \
  pdflatex %s && evince %s &"
                             texfile 
			     idxfile
			     texfile pdffile)))))

(defun pbook-buffer-view-dvi ()
  "Generate and display DVI from the current buffer.
The intermediate files are created in the standard temporary
directory."
  (interactive)
  (save-window-excursion
    (pbook-buffer))
  (with-current-buffer "*pbook*"
    (let ((texfile (pbook-tmpfile "pbook" "tex"))
          (dvifile (pbook-tmpfile "pbook" "dvi"))
	  (idxfile (pbook-tmpfile "pbook" "idx")))
      (write-region (point-min) (point-max) texfile)
      ;; Possibly there is a better way to ensure that LaTeX generates
      ;; the table of contents correctly than to run it more than
      ;; once, but I don't know one.
      (shell-command (format "\
  cd /tmp; latex %s && \
  makeindex %s && \
  latex %s && xdvi %s &"
                             texfile 
			     idxfile
			     texfile dvifile)))))

(defun pbook-buffer-regenerate-dvi ()
  (interactive)
  (save-window-excursion
    (pbook-buffer))
  (with-current-buffer "*pbook*"
    (let ((texfile (pbook-tmpfile "pbook" "tex"))
	  (idxfile (pbook-tmpfile "pbook" "idx")))
      (write-region (point-min) (point-max) texfile)
      ;; Possibly there is a better way to ensure that LaTeX generates
      ;; the table of contents correctly than to run it more than
      ;; once, but I don't know one.
      (shell-command (format "\
  cd /tmp; latex %s && \
  makeindex %s && \
  latex %s"
                             texfile 
			     idxfile
			     texfile)))))

(defun pbook-tmpfile (name extension)
  "Return the full path to a temporary file called NAME and with EXTENSION.
An appropriate directory is chosen and the PID of Emacs is inserted
before the extension."
  (format "%s%s-%S.%s"
          (if (boundp 'temporary-file-directory)
              temporary-file-directory
            ;; XEmacs does it this way instead:
            (temp-directory))
          name (emacs-pid) extension))

;;;# Configurable variables
;;;
;;; These are variables that can be customized to affect pbook's
;;; behaviour. The default regular expressions assume Lisp-style
;;; comment characters, but they can be overridden with buffer-local
;;; bindings from hooks for other programming modes. The other
;;; variables that control formatting are best configured with Emacs's
;;; magic "file variables" (see down the very bottom for an example).

(defvar pbook-commentary-regexp "^;;;\\($\\|[^#]\\)"
  "Regular expression matching lines of high-level commentary.")

(defvar pbook-heading-regexp "^;;;\\(#+\\)"
  "Regular expression matching heading lines of chapters/sections/headings.")

(defvar pbook-heading-level-subexp 1
  "The subexpression of `pbook-heading-regexp' whose length indicates nesting.")

(defvar pbook-include-toc t
  "When true include a table of contents.")

(defvar pbook-style 'article
  "Style of output. Either article (small) or book (large).")

(defvar pbook-author (user-full-name)
  "The name to use in the \author LaTeX command.")

;;;## Configuration variables for code formatting

(defvar pbook-code-prologue "\
\\vspace{1pc}
\\begin{adjustwidth}{0in}{-1.5in}
\\begin{flushleft}
"
  "Tex string to prepend to code listings")

(defvar pbook-code-epilogue "\
\\end{flushleft}
\\end{adjustwidth}
\\vspace{1pc}
"
  "Tex string to append to code listings")

(defvar pbook-current-line nil
  "Holds the line number being processed. Note that this
is reset for every new section of code. This variable
is only accessible while processing code lines, obviously.")

(defvar pbook-current-total-lines nil
  "Holds the total number of lines in the section of
code that's being processed.")

(defun pbook-around-code-line (line-number total-lines)
  "returns a list of Tex strings `(prepend append)' to 
surround the line in a code listing. It may also append
any number of entries to put in `pbook-escaping-regexps'.
It receives, as its arguments, the line number and the
total number of lines in the code segment."
  (labels ((repeat (string num)
	     (if (<= num 0)
		 ""
	       (concat string (repeat string (1- num))))))
    (if (looking-at "^ *$")
	(list "~" "\\\\")
      (let ((str (number-to-string (1+ line-number))))
	(list (concat "\\hspace{-.4in}{\\small\\texttt{"
		      (repeat "\\ " (max 1 (- 4 (length str))))
		      str
		      "\\ \\ "
		      "}}")
	      (if (= line-number (- total-lines 1))
		  "\\\\"
		 "\\nopagebreak[4]\\\\"))))))

(defvar pbook-dark-colors '("black")
  "List of dark colours. Used by the coloring property
to detect when to flip luminances.")

(defvar pbook-face-latex-properties '()
  "plist of latex properties for current face 
 (only active while calling functions in `pbook-properties')")

(defvar pbook-monochrome t
  "Force every color to be the specified color (list of rgb components)
or t for standard black. nil for normal colors.")

(defvar pbook-font-lock-override '(("\\(\\.\\|\\w\\)\\{2,\\}"
				    0 (string-to-syntax "word") ;; was `pbook-identifier'
				    keep t))
  "Appended to `font-lock-syntactic-keywords' while fontifying.")

;;;## configuration for the translation of properties to Latex
;;; 
;;; The code formatting engine is composed of two parts:
;;; a system of properties that link font-lock faces and syntactic
;;; markup with latex environments, and a string escaping function.
;;; These variables let us easily customise both of these systems.

(defvar pbook-current-text-properties nil
  "text-properties of the current region of text. Maybe be used 
by the property transformation functions to fine-tune their
output.")

;;; The most common customisation will be to change the way faces
;;; are shown on paper. By default, they are translated faithfully:
;;; color, slantedness and boldness are directly translated.
;;; While a value of `nil' means don't care (and defers to other
;;; faces properties or default property values), `:no', by default,
;;; disables the associated property.

(defvar pbook-face-override '((font-lock-keyword-face :bold t :index :no)
			      (font-lock-builtin-face :bold t :index :no)
			      (font-lock-function-name-face
                                                      :sc t :tt :no
						      :index function)
			      (font-lock-variable-name-face
			                              :sc t :tt :no
						      :index variable)
			      (font-lock-warning-face :bold t :index :no)
			      (font-lock-comment-face :italic t :tt :no :index :no)
			      (font-lock-doc-face     :italic t :tt :no :index :no)
			      (font-lock-constant-face :italic :no :index :no)
			      (font-lock-string-face  :index :no)
			      (paren-face             :intensity .5)
			      (default                :italic :no))
  "Alist that associates a face with a set of default properties.")

(defvar pbook-escaping-regexps '(("<" . "\\\\textless{}")
				 (">" . "\\\\textgreater{}")
				 ("\\\\" . "\\\\textbackslash{}")
				 ("~" . "\\\\textasciitilde{}")
				 ("\\^" . "\\\\textasciicircum{}")
				 ("[#%&$_{}]" . "\\\\\\&")) ;;space added as needed in pbook-latex-escape
  "alist of regexp -> replacement (passed to re-search-forward and replace-match)
A simple way to index FIXMEs would be to add a regex for that in this list.")

;;; `pbook-properties' defines pbook properties: their name,
;;; when they are applied (by default), and how they are translated
;;; into Latex.
;;; It is a list of triples `([name] [default-value] [translater])'.
;;; `[name]' is an unique identifier for the property.
;;; `[default-value]' is an unary function that, given the face,
;;; returns the value to associate with the property (or `nil' 
;;; to defer).
;;; `[translater]' is either an unary function that, given
;;; the property's value, returns a list of a string to prepend
;;; to the formatted region, a string to append to it, and
;;; any number of pairs as in `pbook-escaping-regexps'.
;;; Latex code is spliced outside (for the first property)
;;; in (for the last property).

(defvar pbook-properties
  `((:color 
     pbook-face-color
     (lambda (color)
       (if (or (null color)
	       (eq pbook-monochrome t)
	       (every (lambda (component)
			(< component 0.01))
		      color))
	   nil
	 (let ((components (mapcar (lambda (component)
				     (if (< component 0.01)
					 "0"
				       (number-to-string component)))
				   (or pbook-monochrome
				       color))))
	   (list (format "\\textcolor[rgb]{%s, %s, %s}{"
			 (first components)
			 (second components)
			 (third components))
		 "}")))))
    
    (:intensity 
     (lambda (face)
       nil)
     (lambda (intensity)
       (if (null intensity)
	   nil
	 (let* ((default-color (if (or (null pbook-monochrome)
				       (eq pbook-monochrome t))
				   '(0.0 0.0 0.0)
				 pbook-monochrome))
		(yuv-default   (apply 'pbook-rgb-yuv default-color))
		(yuv-intensity (list* (- 1 (* intensity (- 1 (car yuv-default))))
				      (cdr yuv-default)))
		(rgb-intensity (apply 'pbook-yuv-rgb yuv-intensity)))
	   (list (apply 'format 
			"\\textcolor[rgb]{%s, %s, %s}{"
			rgb-intensity)
		 "}")))))

    (:bold   face-bold-p
	     (lambda (prop)
	       (and prop
		    (not (eq prop :no))
		    (looking-at " *[^ ]")
		    '("\\textbf{" "}"))))

    (:italic face-italic-p
	     (lambda (prop)
	       (and prop
		    (not (eq prop :no))
		    (looking-at " *[^ ]")
		    '("\\textit{" "}"))))

    (:tt     (lambda (face)
	       t)
	     (lambda (prop)
	       (when (or (not (looking-at " *[^ ]")) ;; always \tt whitespace.
			 (and prop
			      (not (eq prop :no))))
		 '("\\texttt{" "}"))))

    (:sc     (lambda (face)
	       nil)
	     ("\\textsc{" "}"))

    (:index  (lambda (face)
	       (and (eq face 'default)
		    (equal (plist-get pbook-current-text-properties
				      'syntax-table)
			   (string-to-syntax "word"))
		    'use))
	     (lambda (index)
	       (when (and index
			  (not (eq index :no)))
		 (let* ((pbook-escaping-regexps (list* (cons "[!@|]" "\"\\&")
						       pbook-escaping-regexps))
			(word (pbook-latex-escape-string (buffer-string))))
		   (list (format "\\index{%s%s}{" word (ecase index
							 ((function) "|bb")
							 ((variable) "|ii")
							 ((use) "")))
			 "}")))))
    )
  "Complex system. See paragraph above.")

(defun pbook-face-color (face)
  "Given a face, return a triplet of rgb values. Flips the luminance
as needed (to adapt dark background colours to a light background)."
  (let ((dark-bg-p (or (and (boundp 'face-background-mode)
			    (eq face-background-mode 'dark))
		       (member (face-background face)
			       pbook-dark-colors))))
    (and (face-foreground face)
	 (let ((rgb-specs (mapcar (lambda (n)
				    (/ n 65535.0))
				  (color-values (face-foreground face)))))
	   (and rgb-specs
		(if dark-bg-p
		    (let ((yuv-specs (apply 'pbook-rgb-yuv rgb-specs)))
		      (pbook-yuv-rgb (* 0.25 (- 1 (first yuv-specs)))
				     (second yuv-specs)
				     (third yuv-specs)))
		  rgb-specs))))))

(defun pbook-rgb-yuv (r g b)
  "As http://en.wikipedia.org/wiki/YUV -- Matrix fixed by mjp."
  (let* ((y (+ (*  0.299    r) 
	       (*  0.587    g) 
	       (*  0.114    b)))
	 (u (+ (* -0.168740 r)
	       (* -0.331260 g)
	       (*  0.500000 b)))
	 (v (+ (*  0.500000 r)
	       (* -0.418690 g)
	       (* -0.081310 b))))
    (list y u v)))

(defun pbook-yuv-rgb (y u v)
  "As http://en.wikipedia.org/wiki/YUV -- Matrix fixed by mjp" 
  (let ((r (+ y
	      
	      (*  1.40200 v)))
	(g (+ y
	      (* -0.34413 u)
	      (* -0.71414 v)))
	(b (+ y
	      (*  1.77200 u))))
    (mapcar (lambda (x)
	      (cond ((< x 0) 0.0) ;; Need to clamp values for some reason
		    ((> x 1) 1.0)
		    (t       x)))
	    (list r g b))))
;;;# Top-level logic
;;;
;;; Here we have the top level of the program. Setting up, calling the
;;; formatting engine, piecing things together, and putting on the
;;; finishing touches.
;;;
;;; The real work is done in a new buffer called *pbook*. First the
;;; source is fontified, then copied into this buffer and from there
;;; it is massaged into shape.
;;;
;;; Most of this is mundane, but there is one tricky part: the source
;;; buffer may have buffer-local values for some pbook settings, and
;;; we have to be careful or we'd lose them when switching into the
;;; *pbook* buffer. This is taken care of by moving the correct values
;;; of all the relevant customizable settings into new dynamic
;;; bindings.

(defun pbook-process-buffer ()
  "Generate pbook output for the current buffer
The output is put in the buffer *pbook* and displayed."
  (interactive)
  (let ((font-lock-syntactic-keywords
	 (append font-lock-syntactic-keywords
		 pbook-font-lock-override)))
    (setq font-lock-fontified nil) ;; pretend buffer isn't fontified
    (font-lock-default-fontify-buffer)  ;; HACK!!! Looks like an internal...
    )
  (let ((buffer    (current-buffer))
        (beginning (pbook-tex-beginning))
        (ending    (pbook-tex-ending))
        (text      (buffer-string)))
    (with-current-buffer (get-buffer-create "*pbook*")
      ;; Setup,
      (pbook-inherit-buffer-locals buffer
                                   '(pbook-commentary-regexp
                                     pbook-heading-regexp
                                     pbook-style
				     pbook-heading-level-subexp
				     pbook-include-toc
				     pbook-monochrome
				     pbook-font-lock-override
				     pbook-face-override))
      (erase-buffer)
      (insert text)
      ;; Reformat as LaTeX,
      (pbook-preprocess)
      (pbook-format-buffer)
      ;; Insert header & footer.
      (goto-char (point-min))
      (insert beginning)
      (goto-char (point-max))
      (insert ending)
      (display-buffer (current-buffer)))))

(defun pbook-inherit-buffer-locals (buffer variables)
  "Make buffer-local bindings of VARIABLES using the values in BUFFER."
  (dolist (v variables)
    (set (make-local-variable v)
         (with-current-buffer buffer (symbol-value v)))))

(defun pbook-preprocess ()
  "Cleanup the buffer to prepare for formatting."
  (goto-char (point-min))
  ;; FIXME: Currently we just zap all pagebreak characters.
  (save-excursion
    (while (re-search-forward "\C-l" nil t)
      (replace-match "")))
  (unless (re-search-forward pbook-heading-regexp nil t)
    (error "File must have at least one heading."))
  (beginning-of-line)
  ;; Delete everything before the first heading.
  (delete-region (point-min) (point)))

(defun pbook-tex-beginning ()
  "Return the beginning prelude for the LaTeX output."
  (format "\
\\documentclass[notitlepage,a4paper]{%s}
\\usepackage[nohead,nofoot]{geometry}
\\usepackage{color}
\\usepackage{bold-extra}
\\usepackage{chngpage}
\\usepackage{index}
\\newcommand{\\ii}[1]{{\\it #1}}
\\newcommand{\\bb}[1]{{\\bf #1}}
\\makeindex
\\title{%s}
\\author{%s}
\\begin{document}
\\maketitle
%s\n"
          (symbol-name pbook-style)
          (pbook-latex-escape-string (buffer-name))
          (pbook-latex-escape-string pbook-author)
          (if pbook-include-toc "\\tableofcontents" "")))

(defun pbook-tex-ending ()
  "Return the ending of the LaTeX output."
  "\
\\printindex

\\end{document}\n")

;;;# Escaping special characters
;;;
;;; We have to escape characters that LaTeX treats specially. This is
;;; done based on `pbook-escaping-regexps', whicn is defined according
;;; to the rules in the `Special Characters' node of the
;;; LaTeX2e info manual. (CHECKME)

(defun pbook-latex-escape-string (string &optional space)
  (with-temp-buffer
    (insert string)
    (pbook-latex-escape (point-min) (point-max) space)
    (buffer-string)))

(defun pbook-latex-escape (start end &optional space)
  "LaTeX-escape special characters in the region from START to END."
  (when (or space
	    pbook-escaping-regexps)
    (let* ((pbook-escaping-regexps (if space
				       (append pbook-escaping-regexps
					       (list (cons " " "\\\\\\&")))
				     pbook-escaping-regexps))
	   (scan-regexp (apply 'concat 
			      (car (first pbook-escaping-regexps))
			      (mapcan (lambda (entry)
					(list "\\|"
					      (car entry)))
				      (rest pbook-escaping-regexps)))))
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (while (re-search-forward scan-regexp
				    nil t)
	    (goto-char (match-beginning 0))
	    (catch 'out
	      (dolist (entry pbook-escaping-regexps)
		(let ((test (car entry))
		      (replace (cdr entry)))
		  (when (looking-at test)
		    (replace-match replace)
		    (throw 'out nil)))))))))))

;;;# Processing engine
;;;
;;; The main loop scans through the source buffer piece by piece and
;;; converts each one to LaTeX as it goes. There are three sorts of
;;; pieces: headings, top-level commentary, and code.
;;;
;;; This loop recognises what type of piece is at the point and then
;;; calls the appropriate subroutine. The subroutines are responsible
;;; for determining where their piece finishes and for advancing the
;;; point beyond the region they have formatted.

(defun pbook-format-buffer ()
  (while (not (eobp))
    (if (looking-at "^\\s *$")
        ;; Skip blank lines.
        (forward-line)
      (cond ((looking-at pbook-heading-regexp)
             (pbook-do-heading))
            ((looking-at pbook-commentary-regexp)
             (pbook-do-commentary))
            (t
             (pbook-do-code))))))

;;;## Heading formatting
;;;
;;; Each heading line is converted to a LaTeX sectioning command. The
;;; heading text is escaped.

(defun pbook-do-heading ()
  ;; NB: `looking-at' sets the Emacs match data (for match-string, etc)
  (assert (looking-at pbook-heading-regexp))
  (let ((depth (length (match-string-no-properties pbook-heading-level-subexp))))
    ;; Strip off the comment characters and whitespace.
    (replace-match "")
    (when (looking-at "\\s +")
      (replace-match ""))
    (pbook-latex-escape (line-beginning-position) (line-end-position))
    (wrap-line (format "\\%s{" (pbook-nth-sectioning-command depth))
               "}"))
  (forward-line))

(defun wrap-line (prefix suffix)
  "Insert PREFIX at the start of the current line and SUFFIX at the end."
  (save-excursion
    (goto-char (line-beginning-position))
    (insert prefix)
    (goto-char (line-end-position))
    (insert suffix)))

;;; LaTeX has different sectioning commands for articles and books, so
;;; we have to choose from the right set. These variables define the
;;; sets in order of nesting -- the first element is top-level, etc.

(defconst pbook-article-sectioning-commands
  '("section" "subsection" "subsubsection")
  "LaTeX commands for sectioning articles.")

(defconst pbook-book-sectioning-commands
  (cons "chapter" pbook-article-sectioning-commands)
  "LaTeX commands for sectioning books.")

(defun pbook-nth-sectioning-command (n)
  "Return the sectioning command for nesting level N (top-level is 1)."
  (let ((commands (ecase pbook-style
                    (article pbook-article-sectioning-commands)
                    (book    pbook-book-sectioning-commands))))
    (nth (min (1- n) (1- (length commands))) commands)))

;;;## Commentary formatting
;;;
;;; Top-level commentary is stripped of its comment characters and we
;;; escape all characters that LaTeX treats specially.

(defun pbook-do-commentary ()
  "Format one or more lines of commentary into LaTeX."
  (assert (looking-at pbook-commentary-regexp))
  (let ((start (point)))
    ;; Strip off comment characters line-by-line until end of section.
    (while (or (looking-at pbook-commentary-regexp)
               (and (looking-at "^\\s *$")
                    (not (eobp))))
      (replace-match "")
      (delete-horizontal-space)
      (forward-line))
    (save-excursion
      (pbook-latex-escape start (point))
      (pbook-pretty-commentary start (point)))))

;;; These functions define a simple Wiki-like markup language for
;;; basic formatting.

(defun pbook-pretty-commentary (start end)
  "Make commentary prettier."
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (save-excursion (pbook-pretty-tt))
    (save-excursion (pbook-pretty-doublequotes))))

(defun pbook-pretty-tt ()
  "Format `single quoted' text with a typewriter font."
  (while (re-search-forward "`\\([^`']*\\)'" nil t)
    (replace-match "{\\\\tt \\1}" t)))

(defun pbook-pretty-doublequotes ()
  "Format \"double quoted\" text with ``double single quotes''."
  (while (re-search-forward "\"\\([^\"]*\\)\"" nil t)
    (replace-match "``\\1''")))

;;;## Source code formatting
;;;
;;; Source text is rendered as defined in pbook-properties.

(defun pbook-do-code ()
  (assert (and (not (looking-at pbook-commentary-regexp))
               (not (looking-at pbook-heading-regexp))))
  (let ((start (point))
	(end   (progn
		 (pbook-goto-end-of-code)
		 (point))))
    (save-restriction
      (narrow-to-region start end)
      (pbook-convert-tabs-to-spaces start end)
      ;;delete trailing newlines and spaces
      (goto-char (point-max))
      (while (or (equal (char-syntax (char-before)) " ")
		 (bolp))
	(delete-char -1))
      (pbook-format-code start (point-max) 
			 (count-lines start (point-max)))
      (goto-char (point-min))
      (insert pbook-code-prologue)
      (goto-char (point-max))
      (insert "\n" pbook-code-epilogue "\n"))))

(defun pbook-goto-end-of-code ()
  "Goto the end of the current section of code."
  (if (re-search-forward (format "\\(%s\\)\\|\\(%s\\)"
                                 pbook-heading-regexp
                                 pbook-commentary-regexp)
                         nil t)
      (beginning-of-line)
    (goto-char (point-max))))

(defun pbook-convert-tabs-to-spaces (start end)
  "Replace tab characters with spaces."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (untabify start end))))


(defun pbook-format-code (start end num-lines)
  "Format the section of code. The third argument is
the total number of line in the section."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((cur-line 0))
	(while (< cur-line num-lines)
	  (pbook-format-line (line-beginning-position) (line-end-position)
			     cur-line num-lines)
	  (incf cur-line)
	  (beginning-of-line 2))))))

(defun pbook-get-inherits (face)
  "Flattens a face's inheritance list, in order."
  (let ((faces (cond ((eq face 'unspecified) nil)
		     ((listp face)           face)
		     (t                      (list face)))))
    (mapcan (lambda (face)
	      (let ((inherits (face-attribute face :inherit)))
		(if (and inherits
			 (not (eq inherits 'unspecified)))
		  (cons face (pbook-get-inherits inherits))
		(list face))))
	    faces)))

(defun pbook-translate-face-properties (face props)
  "Updates the list of properties `props' with those
associates with `face'. `pbook-face-override' has priority"
  (setq props (append props
		      (copy-list (cdr (assoc face pbook-face-override)))))
  (dolist (defn pbook-properties)
    (let ((prop-name (first defn))
	  (predicate (second defn)))
      (unless (plist-get props prop-name)
	(let ((value (funcall predicate face)))
	  (when value
	    (setq props (plist-put props prop-name value)))))))
  props)

(defun pbook-face-properties (face)
  "Finds a face's (and those from which it inherits) pbook
properties. Earlier faces in the inheritance list (preorder 
depth-first) have priority."
  (let ((faces (append (pbook-get-inherits face)
		       '(default)))
	(props nil))
    (dolist (face faces props)
      (setq props (pbook-translate-face-properties face props)))))

(defun pbook-properties-latex-strings (plist)
  "Given a plist of pbook properties, finds the latex
strings with which to wrap the text that is being formatted,
and the additional regexps with which to escape it."
  (let* ((pbook-face-latex-properties plist) ;;special var
	 (prepend nil)
	 (append  nil)
	 (regexps nil) ;;escaping-regexp entry
	 )
    (dolist (property pbook-properties (list (apply 'concat
						    (reverse prepend))
					     (apply 'concat append)
					     regexps))
      (let* ((name         (first property))
	     (transformer  (third property))
	     (foundp (plist-member plist name))
	     (prop   (plist-get plist name)))
	(when foundp
	  (let ((wrap (if (and (listp transformer)
			       (not (eq (first transformer)
					'lambda)))
			  (and prop
			       (not (eq prop :no))
			       transformer)
			(funcall transformer prop))))
	    (when wrap
	      (push (first wrap) prepend)
	      (push (second wrap) append)
	      (setq regexps (append (cddr wrap)
				    regexps)))))))))

(defun pbook-format-line (start end line-number total-lines)
  "Format a complete line of code, by spans of constant text property.
Also wraps it as per `pbook-around-code-line'. Each span is only escaped
at the very end to facilitate examination of the buffer."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let* ((pbook-current-line line-number)
	     (pbook-current-total-lines total-lines)
	     (substr-beg (point-marker))
	     (substr-end (point-marker))
	     (wrap       (pbook-around-code-line line-number total-lines))
	     (pbook-escaping-regexps (append (cddr wrap)
					     pbook-escaping-regexps)))
	(move-marker substr-end
		     (next-char-property-change (marker-position substr-beg)))
	(set-marker-insertion-type substr-beg t)
	(set-marker-insertion-type substr-end t)
	;; Main loop: find spans of constant text properties
	;; then get the latex trings to wrap around it.
	(while (not (equal substr-beg substr-end))
	  (goto-char (marker-position substr-beg))
	  (let ((wrap (save-excursion     ;;DOCUMENT ME
			(save-restriction
			  (narrow-to-region (marker-position substr-beg)
					    (marker-position substr-end))
			  (let ((pbook-current-text-properties
				 (text-properties-at (marker-position substr-beg))))
			    (pbook-properties-latex-strings
			     (pbook-face-properties 
			      (get-char-property (marker-position substr-beg)
						 'face))))))))
	    (insert (first wrap))
	    (let ((pbook-escaping-regexps (append (third wrap)
						  pbook-escaping-regexps)))
	      (pbook-latex-escape (marker-position substr-beg)
				  (marker-position substr-end)
				  t))
	    (goto-char (marker-position substr-end))
	    (insert (second wrap))

	    (move-marker substr-beg (marker-position substr-end))
	    (move-marker substr-end
			 (next-char-property-change 
			  (marker-position substr-beg)))))
	(wrap-line (first wrap)
		   (second wrap))))))

;;;# Prologue and file variables

(provide 'pbook)

;;; We use Emacs's magic `file variables' to make sure pbook is
;;; formatted how it should be:

;; Local Variables:
;; pbook-author:     "Luke Gorrie, with modifications by Paul Khuong"
;; pbook-use-toc:    t
;; pbook-style:      article
;; pbook-monochrome: t
;; End:
