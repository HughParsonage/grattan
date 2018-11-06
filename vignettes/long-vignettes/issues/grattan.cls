\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{grattan}[2018-09-07 v1.4.0 Reports of the Grattan Institute, Melbourne]
% >gswin64 -dBATCH -dNOPAUSE -sDEVICE=png16m -r300 -sOutputFile=./tests/Report%03d.png Report.pdf
% for png files
\LoadClass[%
a4paper,
11pt,
titlepage,
headings=big,
chapterprefix=false,
headsepline,
twocolumn,
numbers=noenddot,
]{scrreprt}

% Continuous numbering
\@ifundefined{counterwithout}{
\RequirePackage{chngcntr}
}{}
\RequirePackage{etoolbox}
\RequirePackage{footmisc}
\RequirePackage{tablefootnote} % for footnotes within tables
\RequirePackage{zref-savepos}

% Record number and page of footnote:
% http://tex.stackexchange.com/questions/348891/determine-which-page-all-footnotes-landed-on#348940 
% This must be before hyperref and after
% etoolbox footmisc tablefootnote
\patchcmd{\@footnotetext}
  {#1}
  {\zsavepos{footnote@@@\thefootnote}\label{footnote@@@\thefootnote}#1}
  {}
  {\ddt}

% avoid non-discretionary hyphens 
\IfFileExists{logos/grattex-2018-09-07.tex}{
  \exhyphenpenalty=2200
  \hyphenpenalty=2000
}{
  % Defaults pre 2018-09-07: retain for backwards compatibility
  \exhyphenpenalty=900
  \hyphenpenalty=800
}

\brokenpenalty=3000

\RequirePackage{cmap}
\RequirePackage{scrhack}
\RequirePackage{xcolor}
\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{tikz}
\RequirePackage{adjustbox}
  \usetikzlibrary{positioning}
  \usetikzlibrary{decorations.text}
  \usetikzlibrary{decorations.pathmorphing}
  \usetikzlibrary{patterns,arrows,decorations.pathreplacing} % for waterfalls
\RequirePackage{float}
\RequirePackage{placeins}
\RequirePackage{afterpage}
\nonfrenchspacing
\iffalse
\RequirePackage[activate=false, expansion, final, kerning=true, spacing=true]{microtype}
\microtypecontext{spacing=nonfrench}
% http://tex.stackexchange.com/questions/303457/setprotrusion-with-helvetica-on-specific-characters
\SetProtrusion
   [ name     = T1-phv,      % the name is optional
     load     = T1-default ] % first load `T1-default` settings
   { encoding = T1,
     family   = phv }        % use for Helvetica family
   {
     \textendash = {-25, }, \textemdash = {-25, }  % cancel out left protrusion
   }
\fi
\RequirePackage[document]{ragged2e}
% ------
% Grattan colors
\definecolor{Orange}{RGB}{243,144,29}
\definecolor{DarkOrange}{RGB}{212,88,42}
\definecolor{OrangeBackground}{RGB}{254,240,222}  % for boxes
\definecolor{Color1}{RGB}{255,224,127}
\definecolor{Color2}{RGB}{255,195,90}
\definecolor{Color3}{RGB}{246,139,51}
\definecolor{Color4}{RGB}{212,88,42}
\definecolor{Color5}{RGB}{160,34,38}
\definecolor{Color6}{RGB}{98,18,20}
\definecolor{theGrey}{RGB}{106,115,123}
\definecolor{AuthorPage}{RGB}{160,34,38}
\definecolor{AuthorGrey}{RGB}{174,174,174}


% Bullets and numbered items
\RequirePackage{enumitem}
% Eliminate left margin
% \setlist[itemize]{leftmargin=*}
% \setlist[enumerate]{leftmargin=*}

% See http://tex.stackexchange.com/questions/8510/reduce-size-of-bullet-character-in-lists
% Purpose to enlarge and subsequently raise the bullet
% see also \renewcommand{\labelitemi}{\raise .5ex\hbox{\tiny$\bullet$}}
\renewcommand{\labelitemi}{\color{Orange}\parbox[c]{.5em}{\small\ensuremath{\bullet}}{\normalsize\strut}}
\renewcommand{\labelenumi}{\color{Orange}{\bfseries \arabic{enumi}.~}}
\renewcommand{\labelitemii}{{\color{Orange}\bfseries \textendash}}
% ------
\RequirePackage{colortbl}
\RequirePackage{array}
\newcommand{\myhline}{\noalign{\global\arrayrulewidth1pt}\hline
                      \noalign{\global\arrayrulewidth1pt}}


\RequirePackage{graphicx}
\RequirePackage{booktabs}
% No widows
\RequirePackage[all]{nowidow}

\RequirePackage[utf8]{inputenc}
\RequirePackage[T1]{fontenc}
  

\RequirePackage[scaled]{helvet}
  \renewcommand{\familydefault}{\sfdefault}

\RequirePackage[framemethod=TikZ]{mdframed}
\RequirePackage{newfloat}
\RequirePackage{caption}
% Cross references.
\RequirePackage{varioref}
\PassOptionsToPackage{hyphens}{url}
\RequirePackage[
  hypertexnames=false, 
  hidelinks
]{hyperref}
\RequirePackage{cleveref}

% For previous page for 'varioref'
% space after second -2 is important here.
\patchcmd\cref@old@@vpageref
{\advance\@tempcnta-2}
{\advance\@tempcnta-2 }{\typeout{patch ok}}{\ERRORpatchFaild}

% Hack for Chapter references
% This ensures that chapter hyperlinks hit the page
% not the baseline below the chapter title.
\newcommand{\topref}[1]% #1 = label
{\hyperlink{page.\getpagerefnumber{#1}}{\getrefnumber{#1}}}

\newcommand*{\Chapref}[1]{\nameCref{#1}~\topref{#1}}
\newcommand*{\Chapsref}[1]{\nameCrefs{#1}~\topref{#1}}
\newcommand*{\Chaprefrange}[2]{\nameCrefs{#1}~\topref{#1} to~\topref{#2}}
\newcommand*{\Chaprefand}[2]{\nameCrefs{#1}~\topref{#1} and~\topref{#2}}

% Footnote layout (koma)
% \makebox left-aligned numbers.
\deffootnote{2.0em}{1.5em}{\makebox[2.0em][l]{\thefootnotemark.\ }}
\addtokomafont{footnotereference}{\small}

% Detect the footnote immediately prior to CenturyFootnote
\newwrite\fnC
\immediate\openout\fnC=\jobname.fn100
\let\oldfootnote\footnote
\renewcommand{\footnote}[1]{\oldfootnote{#1}\immediate\write\fnC{\thefootnote}}
% Placed just before footnote exceeds 100. 
\newcommand*{\CenturyFootnote}{\label{@CenturyFootnote@@@\thefootnote}\zsavepos{@CenturyFootnote@z}\immediate\closeout\fnC\deffootnote{2.4em}{1.9em}{\makebox[2.4em][r]{\thefootnotemark.\ \ }}}


% Make url formatting the same
\urlstyle{same}

\DeclareCaptionFont{Orange}{\color{Orange}}
\DeclareCaptionJustification{nohyphen}{\hyphenpenalty=10000}
\captionsetup{justification=nohyphen, singlelinecheck=false}

% Boxes
\crefname{boxe}{Box}{Boxes}
\Crefname{boxe}{Box}{Boxes}
\DeclareFloatingEnvironment[listname={List of boxes}, name = {Box}]{boxe}

\counterwithout{boxe}{chapter}
\counterwithout{footnote}{chapter}

\mdfdefinestyle{GrattanFrameBox}{%
    linecolor=Orange,
    nobreak=true, % prevents page breaking
    outerlinewidth=0.5pt,
    innertopmargin=0.5\baselineskip,
    innerbottommargin=0.5\baselineskip,
    innerrightmargin=11pt,
    innerleftmargin=11pt,
    backgroundcolor=OrangeBackground
    }
    
\mdfdefinestyle{GrattanFrameBoxUltra}{%
    linecolor=Orange,
    nobreak=true, % prevents page breaking
    outerlinewidth=0.5pt,
    innertopmargin=0.5\baselineskip,
    innerbottommargin=0.75\baselineskip,
    innerrightmargin=11pt,
    innerleftmargin=11pt,
    backgroundcolor=OrangeBackground
    }

% Purpose not merely to provide consistent formatting of 
% box titles but also to restore caption to normal after caption is complete
\newcommand{\@boxcaption}[1]{%
  \captionsetup{labelfont       = {bf, Orange},
                font            = {bf, Orange},
                format          = plain,
                justification   = raggedright,
                singlelinecheck = false, 
                skip            = 0ex, 
                position        = above}
  \caption{#1}
  \captionsetup{format    = plain,
                font      = {small, bf, theGrey},
                labelfont = {small, bf, theGrey}, 
                position  = above, 
                skip      = 0pt}
}

% Issue #27
\AtBeginEnvironment{mdframed}{\renewcommand{\thempfootnote}{\alph{mpfootnote}}}

% The verysmallbox is intended to be a fraction of one column.
\newenvironment{verysmallbox}[3][htb]{%
  \setlength{\currentparskip}{\parskip}% save the value
  \begin{boxe}[#1]
    \begin{mdframed}[style=GrattanFrameBox]%
    \setlength{\parskip}{\currentparskip} % restore the value
    \@boxcaption{#2\label{#3}}
    \RaggedRight
  }{
    \end{mdframed}%
  \end{boxe}} 
    
% The smallbox is a box intended for onecolumn.
\newenvironment{smallbox}[3][p]{%
  \setlength{\currentparskip}{\parskip}% save the value
  \begin{boxe}[#1]
    \begin{minipage}[\textheight]{\linewidth}
      \begin{mdframed}[style=GrattanFrameBox]%
      \setlength{\parskip}{\currentparskip} % restore the value
      \@boxcaption{\zsavepos{smallbox@@@#3}#2\label{#3}\label{smallbox@@@#3}}
      \RaggedRight
}{%
      \end{mdframed}%
    \null\vfill\null%
  \end{minipage}%
\end{boxe}
} 

% smallbox but centred in the one page
\newenvironment{centredsmallbox}[2]{%
  \setlength{\currentparskip}{\parskip}% save the value
  \@dblfloat{boxe}
  \centering
    \begin{minipage}[\textheight]{\columnwidth}
      \begin{mdframed}[style=GrattanFrameBox]%
      \setlength{\parskip}{\currentparskip} % restore the value
      \@boxcaption{#1\label{#2}}
      \RaggedRight
}{%
      \end{mdframed}%
    \null\vfill\null%
  \end{minipage}%
\end@dblfloat
}

\newcommand{\@addboxcaption}[1]{%
\captionsetup{labelfont={bf,Orange},font={bf,Orange},format=plain,justification=raggedright,singlelinecheck=false, skip=0ex, position=above}
\caption*{#1}
\captionsetup{format=plain,font={small,bf,theGrey},labelfont={small,bf,theGrey}, position=above, skip=0pt}
}

\newenvironment{addsmallbox}[3][htb]{%
  \setlength{\currentparskip}{\parskip}% save the value
  \begin{boxe}[#1]
  \begin{mdframed}[style=GrattanFrameBox]%
  \setlength{\parskip}{\currentparskip}% restore the value
  \@addboxcaption{#2\label{#3}}%
  \RaggedRight
}{
  \end{mdframed}%
  \end{boxe}%
}

% Big boxes -- over the whole page
% It is recommended to surround this with an \afterpage{% 
\newenvironment{bigbox}[2]{
\begin{bigbox*}{#1}{#2}
}{
\end{bigbox*}}

% dbl floats
\newenvironment{bigbox*}[2]{%
  \setlength{\currentparskip}{\parskip}
  \@dblfloat{boxe}%
  \begin{mdframed}[style=GrattanFrameBox]
  \@boxcaption{#1\label{#2}}%
  % Reduced column sep 
  \addtolength{\columnsep}{-23.8pt}%
  \begin{multicols}{2}
  \setlength{\parskip}{\currentparskip}% restore the value
  \RaggedRight
}{%
  \end{multicols}\end{mdframed}
  \end@dblfloat
}
 
\newenvironment{ultrabox}[2]{\onecolumn\setlength{\currentparskip}{\parskip}
\begin{boxe}
\pagecolor{OrangeBackground}
\begin{mdframed}[style=GrattanFrameBoxUltra]%
\setlength{\columnsep}{10mm}
\begin{minipage}[t][\textheight][t]{\textwidth} % textheight
\begin{multicols}{2}
\setlength{\parskip}{\currentparskip}% restore the value
 \captionsetup{labelfont={bf,Orange}, font={bf,Orange}, format=plain,justification=justified,singlelinecheck=false}
 \caption{#1}\label{#2}%
 } {\end{multicols}\end{minipage}\end{mdframed}\end{boxe}%
 \clearpage\twocolumn\nopagecolor}
 

% Captions in general
\DeclareCaptionFont{theGrey}{\color{theGrey}}
\captionsetup{
  format    = plain,
  font      = {small, bf, theGrey},
  labelfont = {small, bf, theGrey}, 
  aboveskip = 1pt
}
% suppress hyphenation in caption
\DeclareCaptionJustification{nohyphen}{\hyphenpenalty=10000}
\captionsetup{justification=nohyphen}

\newcommand{\units}[1]{%
  \captionsetup{font={small,color=theGrey}, aboveskip=0pt, belowskip=0pt}
  \caption*{#1}
}

\newcommand{\captionwithunits}[2]{%
  \captionsetup{format=plain,font={small,bf,theGrey},labelfont={small,bf,theGrey}, justification=raggedright,
  singlelinecheck=false,position=top,skip=0pt}
  \caption{#1}
  \units{#2}
}

\newcommand{\captionoffigurewithunits}[2]{%
  \captionsetup{format=plain,font={small,bf,theGrey},labelfont={small,bf,theGrey}, justification=raggedright,
  singlelinecheck=false}
  \captionof{figure}{#1}
  \vspace{-11pt}
  \captionsetup{font={small,color=theGrey}}
  \caption*{#2}
}

% New user friendly (less typing) \Caption?
\newcommand{\Caption}[3]{\captionwithunits{#1}{#2}\label{#3}}

%% Credit to amsthm.sty
\def\@addpunct#1{%
  \relax\ifhmode
    \ifnum\spacefactor>\@m \else#1\fi
  \fi}

\newcommand*{\source}[1]{%
  \captionsetup{format=plain, font={footnotesize, it}, skip=3pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Source:\ #1\@addpunct{.}}
}

\newcommand*{\notes}[1]{%
  \captionsetup{format=plain, font={footnotesize, it}, skip=3pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Notes:\ #1\@addpunct{.}}
}

\newcommand*{\sources}[1]{%
  \captionsetup{format=plain, font={footnotesize, it}, skip=3pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Sources:\ #1\@addpunct{.}}
}

\newcommand*{\note}[1]{%
  \captionsetup{format=plain, font={footnotesize, it}, skip=3pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Note:\ #1\@addpunct{.}}
}

\newcommand*{\noteswithsource}[2]{%
  \ifstrempty{#2}{\ClassWarning{'\noteswithsource' has second argument empty}}{}%
  \captionsetup{format=plain, font={footnotesize, it}, skip=2.5pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Notes:\ #1\@addpunct{.}}
  \caption*{Source:\ #2\@addpunct{.}}
}

\newcommand*{\notewithsource}[2]{%
  \ifstrempty{#2}{\ClassWarning{'\notewithsource' has second argument empty}}{}%
  \captionsetup{format=plain, font={footnotesize, it}, skip=2.5pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Note:\ #1\@addpunct{.}}
  \caption*{Source:\ #2\@addpunct{.}}
}

\newcommand*{\notewithsources}[2]{%
  \ifstrempty{#2}{\ClassWarning{'\notewithsources' has second argument empty}}{}%
  \captionsetup{format=plain, font={footnotesize, it}, skip=2.5pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Note:\ #1\@addpunct{.}}
  \caption*{Sources:\ #2\@addpunct{.}}
}

\newcommand*{\noteswithsources}[2]{%
  \ifstrempty{#2}{\ClassWarning{'\noteswithsources' has second argument empty}}{}%
  \captionsetup{format=plain, font={footnotesize, it}, skip=2.5pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Notes:\ #1\@addpunct{.}}
  \caption*{Sources:\ #2\@addpunct{.}}
}

\newcommand*{\boxsources}[1]{\footnotesize\textit{#1}\@addpunct{.}}

% More consistent fonts for quotes:
\AtBeginEnvironment{quote}{\small\justifying}

\setkomafont{disposition}{\color{Orange}}
\addtokomafont{chapter}{\bfseries\Large}
\addtokomafont{section}{\bfseries\normalsize}
\addtokomafont{subsection}{\bfseries\normalsize}
\addtokomafont{subsubsection}{\normalsize}

% Spacing
\RedeclareSectionCommand[
  beforeskip=1\baselineskip,
  afterskip=1\baselineskip]{chapter}
\RedeclareSectionCommand[
  beforeskip=0.5\baselineskip plus 0.3\baselineskip,
  afterskip=0.5\baselineskip]{section}
\RedeclareSectionCommand[
  beforeskip=0.5\baselineskip plus 0.2\baselineskip,
  afterskip=0.5\baselineskip]{subsection}
\RedeclareSectionCommand[
  beforeskip=0.5\baselineskip plus 0.1\baselineskip,
  afterskip=0.5\baselineskip]{subsubsection}
\RedeclareSectionCommand[
  beforeskip=.5\baselineskip,
  afterskip=-1em]{paragraph}
\RedeclareSectionCommand[
  beforeskip=-.5\baselineskip,
  afterskip=-1em]{subparagraph}
  
\renewcommand*{\@seccntformat}[1]{\csname the#1\endcsname\hspace{1.25em}}

% Removes glue around section titles. 
\RequirePackage{xpatch}
\xapptocmd{\sectionlinesformat}{\vspace*{-\parskip}}{}{\PatchFailed}

% Named appendix: Appendix A not just 'A'
\providecommand*\appendixmore{}
\renewcommand*\appendixmore{%
    \renewcommand*{\chapterformat}{%
      \mbox{\appendixname~\thechapter\autodot:\enskip}%
    }%
}

  
% --------------
%
% Grattan twocolumn
%
%---------------

% N.B. geometry must be after hyperref, and hence cleveref


\RequirePackage[
  landscape,
  twocolumn,
  left=2.30cm, 
  right=2.30cm,
  top=74pt, 
  headsep=22pt,
  bottom=71.5pt,
  headheight=25.5pt, 
  footskip=25.5pt,
]{geometry}

\setlength{\columnsep}{55pt}  
\setlength{\footheight}{18pt}

% Spacing between paragraphs
% <indent> <distance> <last line end space>
\setparsizes{0pt}{0.6\baselineskip plus 0.25\baselineskip minus 0.1\baselineskip}{0pt plus 1fil}

% Space between body text and footnote area:
\addtolength{\skip\footins}{1pt plus 14pt}

% Ensures hyperlinked chapter headings refer to their referent headings
% not the preceding baseline
% Add extra hyper target for chapter: chapter..\thechapter
\renewcommand*{\chapterformat}{%
  \mbox{\raisebox{25pt}[0pt][0pt]{\hypertarget{chapter..\thechapter}{}}% Add 
    \chapappifchapterprefix{\nobreakspace}\thechapter\hspace{1em}\autodot\enskip}%
}

% Update \addcontentsline to jump to new hyper target _only_ if \chapter is used
\patchcmd{\addcontentsline}% <cmd>
  {\Hy@writebookmark}% <search>
  {\ifnum\pdfstrcmp{chapter}{#2}=0 % Chapter mark
     \edef\@currentHref{page.\thepage}%
   \fi
   \Hy@writebookmark}% <replace>
  {}{}% <success><failure>


  
 % Requests forcefully that figures refrain from
% appearing in the first column.  This will fail
% should a \clearpage be experienced.
\g@addto@macro\@floatplacement{%
\if@firstcolumn
\global\@colnum\z@
\fi
}

\g@addto@macro\@floatplacement{%
\if@firstcolumn
\@fpmin\textheight
\fi
}
  
%% ---------------------------------
%  Headers and footers
%% ---------------------------------
\RequirePackage[headsepline=1pt,plainheadsepline,footsepline=1pt,plainfootsepline]{scrlayer-scrpage}



\clearscrheadings
\clearscrplain
\clearscrheadfoot
% -------
% Heads and footers
%
\RequirePackage{eso-pic}
\newcommand{\classification}{}

% Use Embargo text if declared
\newcommand*{\@EmbargoText}{\null}
\newcommand*{\@EmbargoTitleText}{\null}
\DeclareOption{embargoed}{
  \newcommand*{\EmbargoDate}[1]{\renewcommand{\@EmbargoDate}{#1}}
  \newcommand*{\@EmbargoDate}{XXXX}
  \newcommand*{\EmbargoText}[1]{\renewcommand{\@EmbargoText}{#1}}
  \renewcommand*{\@EmbargoText}{Embargoed until 9 pm \@EmbargoDate}
  \newcommand*{\EmbargoTitleText}[1]{\renewcommand{\@EmbargoTitleText}{#1}}
  \renewcommand*{\@EmbargoTitleText}{\@EmbargoText}
}

% Avoid spaces or newlines in the following commands' arguments: they 
% may cause errors.
% Furthermore, optional arguments are required: they appear in Chapter 
% heading pages. Leaving them out will reset the titles on pages with 
% \chapter{}
\lehead[\normalfont\textcolor{theGrey}{\mytitle}{\huge\strut}]{\normalfont\textcolor{theGrey}{\mytitle}{\huge\strut}}
\lohead[\normalfont\textcolor{theGrey}{\mytitle}{\huge\strut}]{\normalfont\textcolor{theGrey}{\mytitle}{\huge\strut}}
\rehead[\classification\@EmbargoText]{\classification\@EmbargoText}
\rohead[\classification\@EmbargoText]{\classification\@EmbargoText}
\ifoot[{\textcolor{theGrey}{\normalfont Grattan Institute \@YEAR {\Large\strut}}}]{\textcolor{theGrey}{\normalfont Grattan Institute \@YEAR}{\Large\strut}}
\cfoot[\classification]{\classification}
\ofoot[\normalfont\textcolor{theGrey}{\thepage}]{\normalfont\textcolor{theGrey}{\thepage}}

\setkomafont{headsepline}{\color{Orange}}
\setkomafont{footsepline}{\color{Orange}}


% Figure parameters -----------------------
%\renewcommand{\floatpagefraction}{0.75}

% length of fptop is magical: equal to the 
\newlength{\@chaproom}
\setlength{\@chaproom}{33.64366pt}
\setlength{\@fptop}{36.64366pt minus 36.64366pt}
\let\oldtryfcolumn=\@tryfcolumn
\def\@tryfcolumn{\addtolength{\@fptop}{\dimexpr \@colht-\textheight}%
  \oldtryfcolumn}
\setlength{\@fpbot}{36.64366pt plus 1fil minus 36.64366pt}
\setkeys{Gin}{width=\columnwidth}

\newenvironment{figureTop}{
\begin{figure}
\begin{minipage}[t][\textheight]{\linewidth}
\vspace{1pt}
}{%
\end{minipage}
\end{figure}
}

% For two figures on one page (with possible space for text underneath)
\newcommand{\doublecolumnfigure}[2]{
  \begin{figure*}
    \begin{minipage}[t][\textheight]{\columnwidth}
      \vspace{\@fptop}
      #1
    \end{minipage}
    \hfill
    \begin{minipage}[t][\textheight]{\columnwidth}
      \vspace{\@fptop}
      #2
    \end{minipage}
  \end{figure*}
}

% Ensure pdfs are used first
\DeclareGraphicsExtensions{%
    .pdf,.PDF,%
    .png,.PNG,%
    .jpg,.mps,.jpeg,.jbig2,.jb2,.JPG,.JPEG,.JBIG2,.JB2}

%% FrontPage options
%% First, the default, plain front page with a title.
\newcommand*{\titleTH}{\begingroup % Create the command for including the title page in the document
\raggedleft % Right-align all text
\vfil
\phantom{.}\hfill 

\IfFileExists{GrattanSVGLogo.pdf}{\includegraphics[width=0.225\paperwidth,keepaspectratio]{GrattanSVGLogo}}{\includegraphics[width=0.225\paperwidth,keepaspectratio]{./logos/GrattanSVGLogo}}
\vspace{30pt}
\vfil % Whitespace at the top of the page
{\bfseries\textcolor{Orange}{\fontsize{30}{35}\selectfont \mytitle}}\\[\baselineskip] % Title page font size
{\bfseries\fontsize{18}{35}\selectfont \textcolor{Orange}{\mysubtitle}}\\[\baselineskip]

{\LARGE \myauthor}

\vfil

{\LARGE\bfseries\@EmbargoTitleText\hfill\null}

\vfill% Whitespace at the bottom of the page
\endgroup}
\newcommand{\BackgroundPic}{}

% Alternatively, if FrontPage is declared, the file there is used as the frontpage picture
\DeclareOption{FrontPage}{
  \IfFileExists{./FrontPage/FrontPage.pdf}{%
      %\includegraphics[width=\paperwidth,keepaspectratio]{./FrontPage/FrontPage}%
    }{
      \ClassError{grattan}{%
      './FrontPage/FrontPage.pdf' does not exist
      }{%
      You have asked for a fullpicture option, but we can't find 
      a file at './FrontPage/FrontPage.pdf'. Try placing the image
      there or not using the fullpicture option.
    }
    }
  \renewcommand\BackgroundPic{%
    \put(0,0){%
    \parbox[b][\paperheight]{\paperwidth}{%
    \vfill
    \centering
    % Amend
    \IfFileExists{./FrontPage/FrontPage.pdf}{%
      \includegraphics[width=\paperwidth,keepaspectratio]{./FrontPage/FrontPage.pdf}%
    }{
      \ClassError{grattan}{%
        './FrontPage/FrontPage' does not exist
      }{%
        You have asked for a fullpicture option, but we can't find 
        a file at './FrontPage/FrontPage'. Try placing the image
        there or not using the fullpicture option.
      }
    }
  \vfill
  }}}
  \renewcommand{\titleTH}{} % FrontPage should be entire.

}

\DeclareOption{continuous}{%
  \KOMAoption{listof}{nochaptergap}
  \addtocontents{lof}{\linespread{1.3}\selectfont}% optical
  \addtocontents{lot}{\linespread{1.3}\selectfont}% optical
  \counterwithout{figure}{chapter}
  \counterwithout{table}{chapter}
}

\newcommand*{\@pagetwo}{%
  \begin{minipage}[t][0.85\textheight][t]{\columnwidth}
  \subsection*{Grattan Institute Support}  
  \begin{multicols}{2}
  \setlength{\parskip}{4pt plus 8pt minus 5pt}
  \textbf{\textcolor{theGrey}{\normalsize Founding members}}\hfill\par
  \vspace{-40pt} 
  {\centering  
  
  \IfFileExists{aus-gov-logo-stacked-black.pdf}%
    {\includegraphics[width=3.75cm]{aus-gov-logo-stacked-black}}%
    {\includegraphics[width=3.75cm]{./logos/aus-gov-logo-stacked-black}}
  
  \IfFileExists{Vic_Gov_Logo-2016.pdf}%
    {\includegraphics[width=3.25cm]{Vic_Gov_Logo-2016}}%
    {\includegraphics[width=3.25cm]{./logos/Vic_Gov_Logo-2016}}
  
  \IfFileExists{UOM-Pos_S_PMS.pdf}%
    % Too much whitespace on top:
    % l t r b
    {\includegraphics[trim = {0 2cm 0 2cm}, clip, width=3.5cm]{UOM-Pos_S_PMS}}%
    {\includegraphics[trim = {0 2cm 0 2cm}, clip, width=3.5cm]{./logos/UOM-Pos_S_PMS}}%

  
  \IfFileExists{Bhp.pdf}%
    {\includegraphics[width=3.25cm]{Bhp}}%
    {\includegraphics[width=3.25cm]{./logos/Bhp}}%
  % par is necessary to centre the last logo
  \par}
  \columnbreak\par
  \footnotesize 
  \textbf{\textcolor{theGrey}{\normalsize Endowment Supporters}}

  The Myer Foundation

  National Australia Bank

  Susan McKinnon Foundation

  \vspace{15pt}

  \textbf{\textcolor{theGrey}{\normalsize Affiliate Partners}}

  Medibank Private

  Susan McKinnon Foundation
  
  \vspace{15pt}

  \textbf{\textcolor{theGrey}{\normalsize Senior Affiliates}}
 
  Google 

  Maddocks

  PwC
  
  McKinsey \& Company

  The Scanlon Foundation

  Wesfarmers

  Westpac

  \vspace{15pt}

  \textbf{\textcolor{theGrey}{\normalsize Affiliates}}

  Ashurst

  Corrs

  GE ANZ

  Jemena

  Urbis

  Woodside\rule[-1.5ex]{0pt}{0pt} % for BHp
  \end{multicols}
  \end{minipage} 
  \eject \normalsize
  \begin{minipage}[t][0.85\textheight][t]{\columnwidth}
    \subsection*{Grattan Institute \@Report\ No. \@GrattanReportNumber, \@MONTH\ \@YEAR}
    \setlength{\parskip}{5.5pt plus 2pt}
    \raggedright\@acknowledgements
  \end{minipage}
  \twocolumn
}

\newcommand*{\@checkGrattanReportNumberdefined}{
  \ifdefempty{\@GrattanReportNumber}%
    {\ClassError{You have not provided a Grattan Report Number}}%
    {}%
}

\DeclareOption{submission}{
  % A submission has no special page two
  \renewcommand*{\@pagetwo}{
    \ifdefempty{\@acknowledgements}{}{\ClassWarning{grattan}{Using option 'submission', but 'acknowledgements' defined.}}
    \ifdefempty{\@GrattanReportNumber}{}{\ClassWarning{grattan}{Using option 'submission', but 'GrattanReportNumber' defined.}} 
  }
  \renewcommand*{\@checkGrattanReportNumberdefined}{}
}

\ProcessOptions\relax

% Bibliography
\RequirePackage[english]{babel}
\RequirePackage{csquotes}
\RequirePackage[
  backend=biber,
  singletitle,
  style=authoryear-ibid,
  ibidtracker=constrict, % avoid ibids from figures being confusing
  autocite=footnote, 
  maxcitenames=2, 
  maxbibnames=9, 
  uniquelist=false, 
  uniquename=init,
  sorting=anyt,
  date=year, % else bibliography will be e.g. ATO (Jul. 10, 2016c)
  labelalpha,
  maxalphanames=1
]{biblatex}

\@ifpackagelater{biblatex}{2016/09/01}{%
% Maintain backwards-compatibility between these dates
  \@ifpackagelater{biblatex}{2017/11/04}{
    \DeclareLabelalphaTemplate{\labelelement{\field{labelname}\field{labelyear}}}
    \DeclareSortingTemplate{gratt}{\sort{\field{labelalpha}}\sort{\field{labelyear}}\sort{\field{author}}}
    \ExecuteBibliographyOptions{sorting=gratt}

    \newtoggle{bbx:dowehavemorenames}
    \DeclareNameFormat{dowehavemorenames}{%
     \ifboolexpr{
       test {\ifnumequal{\value{listcount}}{\value{liststop}}}
       and
       test \ifmorenames
     }
       {\global\toggletrue{bbx:dowehavemorenames}}
       {\global\togglefalse{bbx:dowehavemorenames}}}

    \newcounter{mymaxcitenames}
    \AtBeginDocument{%
     \setcounter{mymaxcitenames}{\value{maxnames}}%
    }

    \renewbibmacro*{begentry}{%
      \begingroup
        \defcounter{maxnames}{\value{mymaxcitenames}}%
        \printnames[dowehavemorenames]{labelname}%
        \iftoggle{bbx:dowehavemorenames}
          {\printnames{labelname}%
           \setunit{\printdelim{nameyeardelim}}%
           \usebibmacro{date+extradate}%
           \space\space\newunit\newblock}
          {}%
      \endgroup
      \iftoggle{bbx:dowehavemorenames}{\renewbibmacro*{date+extradate}{}}{}%
    }
  }{
  %% biblatex version 3.7
    \DeclareLabelalphaTemplate{\labelelement{\field{labelname}\field{labelyear}}}
    \DeclareSortingScheme{gratt}{\sort{\field{labelalpha}}\sort{\field{labelyear}}}
    \ExecuteBibliographyOptions{sorting=gratt}

    \newtoggle{bbx:dowehavemorenames}
    \DeclareNameFormat{dowehavemorenames}{%
     \ifboolexpr{
       test {\ifnumequal{\value{listcount}}{\value{liststop}}}
       and
       test \ifmorenames
     }
       {\global\toggletrue{bbx:dowehavemorenames}}
       {\global\togglefalse{bbx:dowehavemorenames}}}

    \newcounter{mymaxcitenames}
    \AtBeginDocument{%
     \setcounter{mymaxcitenames}{\value{maxnames}}%
    }

    \renewbibmacro*{begentry}{%
      \begingroup
        \defcounter{maxnames}{\value{mymaxcitenames}}%
        \printnames[dowehavemorenames]{labelname}%
        \iftoggle{bbx:dowehavemorenames}
          {\printnames{labelname}%
           \setunit{\printdelim{nameyeardelim}}%
           \usebibmacro{date+extrayear}%
           \space\space\newunit\newblock}
          {}%
      \endgroup
      \iftoggle{bbx:dowehavemorenames}{\renewbibmacro*{date+extrayear}{}}{}%
    }
  }
}%
% do nothing
{}

\DeclareLanguageMapping{english}{british-apa}
\DeclareNameAlias{author}{last-first} % for last name, first name in bibliography
\DeclareFieldFormat{type}{\unskip\space} % suppress Tech Rep
\renewbibmacro{in:}{} % suppress In:
\renewbibmacro{bbx:editor}{} % suppress Ed:


%% Bibliography formatting
% separating entries
\setlength\bibitemsep{1.5\itemsep}
% format bibliography
\renewcommand*{\bibfont}{\small\raggedright}

% Allow URLs to break on any character
  % Increase penalty for page-breaks within entry from 5000 to 10,000 (infinity)
  \patchcmd{\bibsetup}{\interlinepenalty=5000}{\interlinepenalty=10000}{}{}
\let\origbibsetup\bibsetup
\renewcommand{\bibsetup}{%
  \origbibsetup%
  \expandafter\def\expandafter\UrlBreaks\expandafter{\UrlBreaks%  save the current one
    \do\a\do\b\do\c\do\d\do\e\do\f\do\g\do\h\do\i\do\j%
    \do\k\do\l\do\m\do\n\do\o\do\p\do\q\do\r\do\s\do\t%
    \do\u\do\v\do\w\do\x\do\y\do\z\do\A\do\B\do\C\do\D%
    \do\E\do\F\do\G\do\H\do\I\do\J\do\K\do\L\do\M\do\N%
    \do\O\do\P\do\Q\do\R\do\S\do\T\do\U\do\V\do\W\do\X%
    \do\Y\do\Z}
}
\DeclareFieldFormat{url}{\textcolor{blue}{\url{#1}}}


%% Citation tweaking
\DeclareFieldFormat{titlecase}{#1}
\setlength\bibhang{.5in}
\renewcommand*{\bibnamedash}{%
  \ifdimless{\leftmargin}{0.75em}
    {\mbox{\textemdash\space}}
    {\makebox[\leftmargin][l]{%
       \ifdimless{\leftmargin}{1.25em}
         {\textendash}
         {\rule{0.8\bibhang}{.2pt}}}}}
% Hyperlinks entire citation label for most citaiton commands 
% http://tex.stackexchange.com/questions/15951/hyperlink-name-with-biblatex-authoryear-biblatex-1-4b
\DeclareFieldFormat{citehyperref}{%
  \DeclareFieldAlias{bibhyperref}{noformat}% Avoid nested links
  \bibhyperref{#1}}

\DeclareFieldFormat{textcitehyperref}{%
  \DeclareFieldAlias{bibhyperref}{noformat}% Avoid nested links
  \bibhyperref{%
    #1%
    \ifbool{cbx:parens}%
      {\bibcloseparen\global\boolfalse{cbx:parens}}%
      {}}}

\savebibmacro{cite}
\savebibmacro{textcite}

\renewbibmacro*{cite}{%
  \printtext[citehyperref]{%
    \restorebibmacro{cite}%
    \usebibmacro{cite}}}

\newbibmacro*{cite:title}{%
  \ifsingletitle
    {}
    {\printtext[bibhyperref]{%
       \printfield[citetitle]{labeltitle}}}}

\renewbibmacro*{textcite}{%
  \ifboolexpr{%
    ( not test {\iffieldundef{prenote}} and
      test {\ifnumequal{\value{citecount}}{1}} )
    or
    ( not test {\iffieldundef{postnote}} and
      test {\ifnumequal{\value{citecount}}{\value{citetotal}}} )
  }%
    {\DeclareFieldAlias{textcitehyperref}{noformat}}
    {}%
  \printtext[textcitehyperref]{%
    \restorebibmacro{textcite}%
    \usebibmacro{textcite}}}

% The following lines give the correct footcite Author (Year) but make the ibid erroneous
% Author (ibid) % use ifciteibid
\DeclareCiteCommand{\footcite}[\mkbibfootnote]
  {\boolfalse{cbx:parens}}
  {\usebibmacro{citeindex}%
  % https://github.com/grattaninstitute/AP-Housing-affordability-2017/commit/55757ed2c625d4e18ada5a71edfdb5fbd88f56d1
  % Ibid. -> Ibid\adddot
  % Otherwise the . in Ibid. is interprted as the end of a sentence
  % which means the first letter in a postnote will be uppercase
  \ifciteibid{Ibid\adddot}{%  % for ibidem
   \iffirstcitekey
     {\setcounter{textcitetotal}{1}}
     {\stepcounter{textcitetotal}%
      \textcitedelim}%
   \usebibmacro{textcite}}%
   }
  {\ifbool{cbx:parens}
     {\bibcloseparen\global\boolfalse{cbx:parens}}
     {}}
  {\usebibmacro{textcite:postnote}}
\newcommand{\gcite}[1]{{\citeauthor{#1} (\citedate{#1})}}
% removes intersentence space after et al.
%\xpatchcmd\citeauthor{\begingroup}{\begingroup\aftergroup\@}{}{}
%\xpatchcmd\textcite{\begingroup}{\begingroup\aftergroup\@}{}{}
\DefineBibliographyStrings{british}
   {andothers = {et al.\hskip 0pt}}

% biblatex ShareLaTeX version control
% 2016/09/17
\@ifpackagelater{biblatex}{2016/09/01}{\ExecuteBibliographyOptions{giveninits=true}}{\ExecuteBibliographyOptions{firstinits=true}}
% Patch for issue 
\AtBeginDocument{%
  \apptocmd\@floatboxreset
    {\booltrue{citetracker}%
     \booltrue{pagetracker}}
    {}
    {\blx@err@patch{floats}}
}

\RequirePackage{multicol}
\newcommand{\logoskip}{\par\vspace{16pt}}

\newcommand*{\ReportOrWorkingPaper}[1]{\renewcommand*{\@Report}{#1}}
\newcommand{\@Report}{Report}

\newcommand*{\GrattanReportNumber}[1]{\renewcommand*{\@GrattanReportNumber}{#1}}
\newcommand{\@GrattanReportNumber}{}

\AtBeginDocument{%
  \@checkGrattanReportNumberdefined{}
}

%% BibLaTeX needs to precede acknowledgements (otherwise acknowledgements can't contain citations).

% Allow MONTH/YEAR to be defined; otherwise by default the date of compilation
\newcommand*{\MONTH}[1]{\renewcommand*{\@MONTH}{#1}}
\newcommand{\@MONTH}{%
  \ifcase\the\month
  \or January% 1
  \or February% 2
  \or March% 3
  \or April% 4
  \or May% 5
  \or June% 6
  \or July% 7
  \or August% 8
  \or September% 9
  \or October% 10
  \or November% 11
  \or December% 12
  \fi}

\newcommand*{\YEAR}[1]{\renewcommand*{\@YEAR}{#1}}
\newcommand{\@YEAR}{\the\year}

\newcommand\acknowledgements[1]{\renewcommand\@acknowledgements{#1}}
\newcommand\@acknowledgements{}

\newcommand*{\ISBN}[1]{\renewcommand*{\@ISBN}{#1}}
\newcommand{\@ISBN}{}

\RequirePackage{tabularx}
\AtBeginEnvironment{tabular}{\small}
\AtBeginEnvironment{tabularx}{\small}
\newcolumntype{R}{>{\RaggedLeft\arraybackslash}X}
\newcolumntype{Q}{>{\raggedleft\arraybackslash}X}
  
\AtBeginDocument{%
  \pagestyle{empty}
  \let\mytitle\@title
  \let\mysubtitle\@subtitle
  \let\myauthor\@author
  
  % TitlePage
  \onecolumn
  \AddToShipoutPicture*{\BackgroundPic}
  \phantom{.}\hfill\titleTH
  \ClearShipoutPicture
  \clearpage
  \twocolumn
  % use microtype
  
  %
% Following page.
\pagestyle{scrheadings}
\@pagetwo
}

% Table of contents
\setcounter{tocdepth}{0}

\newlength{\currentparskip}

% Designing the overview
% ---
% We define a new environment that take as its contents the text of the 
% overview, placing it with a chapter* heading 'Overview' all on the same
% page. 
\newlength{\overviewExtra}
\setlength{\overviewExtra}{0pt}
\newenvironment{overview}[1][]%
% vspace{-24.5pt} should align Overview baseline with chapter baseline
% Putting \label outside \addchap causes inconsistent chapter breaks
  {\onecolumn\vtop to 0pt\bgroup\ifstrempty{#1}{\vspace{-24.5pt}}{\vspace{#1}}\addchap{Overview\label{chap:Overview}}\addtolength{\columnsep}{\overviewExtra}\begin{multicols}{2}}%
  {\end{multicols}\addtolength{\columnsep}{-\overviewExtra}\vss\egroup\hfill\twocolumn}
  
\newenvironment{summary}%
  {\onecolumn\vtop to 0pt\bgroup\vspace{-25pt}\chapter*{Summary}\begin{multicols}{2}}%
  {\end{multicols}\vss\egroup\hfill\twocolumn}
  
\newenvironment{onepager}[1]%  
  {\onecolumn\vtop to 0pt\bgroup\vspace{-25pt}\chapter*{#1}\begin{multicols}{2}}%
  {\end{multicols}\vss\egroup\hfill\twocolumn}

\newlength{\recommendationExtra}
\setlength{\recommendationExtra}{0pt}
\newenvironment{recommendations}[1][]%
  {\onecolumn\vtop to 0pt\bgroup\ifstrempty{#1}{\vspace{-24.5pt}}{\vspace{#1}}\addchap{Recommendations}\label{chap:Recommendations}\addtolength{\columnsep}{\recommendationExtra}\begin{multicols}{2}}%
  {\end{multicols}\addtolength{\columnsep}{-\recommendationExtra}\vss\egroup\hfill\twocolumn}

\newcounter{reco}
\crefalias{reco}{rec}
\Crefname{reco}{Recommendation}{Recommendations}
\Crefname{rec}{Recommendation}{Recommendations}
\newcommand*{\recommendation}[1]{{\subsection*{Recommendation \refstepcounter{reco}\thereco: #1}}}

\newcounter{subreco}
\newcommand*{\subrecommendation}[1]{{\color{Orange}\refstepcounter{subreco}Rec.\hspace{4pt}\thereco\alph{subreco}: {#1}\hspace{0.4em}}}


\RequirePackage{tocloft}
\renewcommand{\cftchapfont}{\normalsize\normalfont}
\renewcommand{\cftchapdotsep}{4.5}
\renewcommand{\cftchappagefont}{\normalfont}
\renewcommand{\cftdot}{\normalfont .}
\renewcommand{\@pnumwidth}{1em}

% indent of list of figures
\renewcommand{\cftfigindent}{0em}

\AtBeginDocument{\renewcommand{\contentsname}{Table of contents}}
\newcommand{\contentspage}{%
  \onecolumn
  \begin{multicols}{2}
    \phantom{.}
    \vfill
    \columnbreak
    \begingroup
      \raggedright
      \tableofcontents
    \endgroup
  \end{multicols}
  \twocolumn%
}

\newcommand{\oneraggedpage}{\let\mytextbottom\@textbottom
  \let\mytexttop\@texttop
  \raggedbottom
  \afterpage{%
  \global\let\@textbottom\mytextbottom
  \global\let\@texttop\mytexttop}}


% Discretionary commands for consistent treatment of common abbreviations
\newcommand*{\eg}{\emph{e.g.}}
\newcommand*{\ie}{\emph{i.e.}}
\newcommand*{\etc}{\emph{etc.}}

% Helpful for determining which labels are causing infinite reruns.
% https://tex.stackexchange.com/questions/154594/how-to-diagnose-a-permanent-labels-may-have-changed-warning?noredirect=1&lq=1

\def\@testdef #1#2#3{%
  \def\reserved@a{#3}\expandafter \ifx \csname #1@#2\endcsname
 \reserved@a  \else
\typeout{^^Jlabel #2 changed:^^J%
\meaning\reserved@a^^J%
\expandafter\meaning\csname #1@#2\endcsname^^J}%
\@tempswatrue \fi}

% To detect user's current version
\listfiles
