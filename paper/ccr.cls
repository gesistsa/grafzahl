% Template for CCR Articles (very WIP)
% 2023 Wouter van Atteveldt, Damian Trilling, Chung-hong Chan
% Version 0.02
% Please see https://github.com/vanatteveldt/ccr-quarto for the latest version of this file

\ProvidesClass{ccr}[2023-02-03 v0.01]
\NeedsTeXFormat{LaTeX2e}

\LoadClass[twoside]{article}

%%%%%%%%%%%%% OVRERALL PAGE LAYOUT %%%%%%%%%%%%%%%%%%%
\RequirePackage[papersize={6.53in,9.61in},
  left=1.1in,right=1.1in,top=1in,textheight=7.25in]{geometry}

\RequirePackage{graphicx}

\RequirePackage[english]{babel}
\renewcommand*\oldstylenums[1]{\textosf{#1}}
\RequirePackage{ifxetex}
\ifxetex
  \RequirePackage[protrusion=true,final,babel]{microtype}
  \RequirePackage{fontspec}
  \setmainfont{erewhon}  
  \setsansfont{Noto Sans}
  \usepackage{unicode-math}
  \defaultfontfeatures{ Scale=MatchLowercase, Ligatures=TeX }
  \setmathfont{TeX Gyre Termes Math}

\else
  \RequirePackage[protrusion=true,expansion=true,final,babel]{microtype}
  \RequirePackage[sfdefault,scaled=.9]{noto}
  \RequirePackage[proportional,scaled=1]{erewhon}
  \RequirePackage[erewhon,vvarbb,bigdelims]{newtxmath}
\fi
\renewcommand{\floatpagefraction}{.7}
\linespread{1.1}

%%%%%%%%%%%%%% Information from authors %%%%%%%%%%%%%%%%%%%%%
\RequirePackage{ifthen}

\def\@shorttitle{}
\newcommand{\shorttitle}[1]{\def\@shorttitle{#1}}
\newcommand{\show@shorttitle}{%
  \ifthenelse{\equal{\@shorttitle}{}}%
  {\MakeLowercase{\@title}}{\MakeLowercase{\@shorttitle}}%
}

\def\@abstract{(specify abstract using \textbackslash abstract in preamble)}
\renewcommand{\abstract}[1]{\def\@abstract{#1}}
\newcommand{\show@abstract}{\@abstract}

\def\@keywords{(specify keywords using \textbackslash keywords in preamble)}
\newcommand{\keywords}[1]{\def\@keywords{#1}}
\newcommand{\show@keywords}{\@keywords}

\def\@shortauthors{(please specify \textbackslash shortauthors)}
\newcommand{\shortauthors}[1]{\def\@shortauthors{#1}}
\newcommand{\show@shortauthors}{\@shortauthors}

\def\@volume{X}
\newcommand{\volume}[1]{\def\@volume{#1}}
\newcommand{\show@volume}{\@volume}
\def\@pubnumber{Y}
\newcommand{\pubnumber}[1]{\def\@pubnumber{#1}}
\newcommand{\show@pubnumber}{\@pubnumber}
\def\@pubyear{20xx}
\newcommand{\pubyear}[1]{\def\@pubyear{#1}}
\newcommand{\show@pubyear}{\@pubyear}
\def\@doi{10.5117/ccr20xx.xxx.xxxx}
\newcommand{\doi}[1]{\def\@doi{#1}}
\newcommand{\show@doi}{\@doi}


\newcommand{\firstpage}[1]{\setcounter{page}{#1}}
%%%%%%%%%%%%%% HEADER / FOOTER %%%%%%%%%%%%%%%%%%%%%%%%
\RequirePackage{lastpage}
\RequirePackage{fancyhdr}

\newcommand{\smallcaps}[1]{\textsc{\footnotesize #1}}
\newcommand{\smallcapsl}[1]{\MakeLowercase{\smallcaps{#1}}}
\setlength{\headheight}{24pt}

%\usepackage{showframe} % useful for debugging header / margin
\fancypagestyle{firstpage}{%
\fancyhf{} % clear all six fields
\fancyhead[L]{\includegraphics[height=2em]{aup_logo.pdf}}
\fancyhead[R]{\raisebox{.1em}{\smallcaps{computational communication research 
               \oldstylenums{\show@volume}.\oldstylenums{\show@pubnumber} (\oldstylenums{\show@pubyear}) 
               \oldstylenums{\thepage}--\oldstylenums{\pageref{LastPage}}}}%
               \\%
               \raisebox{.4em}{\scriptsize\MakeUppercase{\sc\url{https://doi.org/\show@doi}}}}
\fancyfoot[RE,LO]{\footnotesize© The author(s). This is an open access article distributed under the \href{https://creativecommons.org/licenses/by/4.0/}{\textsc{cc by} \oldstylenums{4.0} license}}
\fancyfoot[LE,RO]{\smallcaps{\thepage}}
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}
}
\fancypagestyle{followingpage}{%
\fancyhf{} % clear all six fields
\fancyhead[RE]{\smallcaps{\show@shorttitle}}
\fancyhead[LO]{\smallcapsl{Computational Communication Research}}
\fancyfoot[LE,RO]{\smallcaps{\thepage}}
\fancyfoot[LO]{\smallcapsl{\show@shortauthors}}
\fancyfoot[RE]{\smallcaps{vol.  \oldstylenums{\show@volume}, no.  \oldstylenums{\show@pubnumber},  \oldstylenums{\show@pubyear}}}
\renewcommand{\headrulewidth}{0Pt}
\renewcommand{\footrulewidth}{0pt}
}

\pagestyle{followingpage}
\AtBeginDocument{\thispagestyle{firstpage}}




%%%%%%%%%%%%%%%%%% Other markup / styling %%%%%%%%%%%%%%%%%%%%%
\RequirePackage{etoolbox}
% Tables
\AtBeginEnvironment{tabularx}{\footnotesize\sffamily}
\AtBeginEnvironment{tabular}{\footnotesize\sffamily}
\renewcommand{\arraystretch}{1.3}

\RequirePackage{makecell}
\renewcommand\theadfont{ \bfseries}
\renewcommand\theadalign{ll}
\usepackage[font={footnotesize,sf}]{caption}

% More compact enumerations
\renewcommand{\@listI}{%
  \itemsep=0\parsep}
% Bibliography style

%%%%%%%%%%%%% Author information  black magic %%%%%%%%%%%%%%%%%
% Mostly stolen from https://github.com/dan-weiss/apa7-latex-cls-source/blob/62f31e0b2c8c75e260a7690928c745d803333549/apa7/apa7.dtx (LPPL licensed)

\newcommand*\listauthors{}
\newcommand*\listsuperscripts{}
\newcommand*\listaffiliations{}

\newcommand*{\authorsnames}[2][]{
  \def\def@multipleauthors{\@multipleauthorsmode} %
  \renewcommand*\listauthors{}
  \renewcommand*\listsuperscripts{}
  \newcounter{NumberOfAuthors}
  \newcounter{NumberOfSuperscripts}
  \forcsvlist{\stepcounter{NumberOfAuthors}\listadd\listauthors}{#2}
  \forcsvlist{\stepcounter{NumberOfSuperscripts}\listadd\listsuperscripts}{#1}
  }
  
\newcommand*{\authorsaffiliations}[1]{
  \def\def@multipleaffils{\@multipleaffilsmode} %
  \renewcommand*\listaffiliations{}
  \newcounter{NumberOfAffiliations}
  \forcsvlist{\stepcounter{NumberOfAffiliations}\listadd\listaffiliations}{#1}
  }
  
\catcode`\|=3
  
\def\looptwo#1#2{%
    \edef\tmp{\noexpand\xtwo%
      \unexpanded\expandafter{#1}\relax  % no added delimiter here
      \unexpanded\expandafter{#2}\relax  % no added delimiter here
    } \tmp%
  }%
  
\def\xtwo#1|#2\relax#3|#4\relax{%
    \dotwo{#1}{#3}%
    \def\tmp{#2}%
    \ifx\empty\tmp%
      \expandafter\@gobble%
    \else%
      \expandafter\@firstofone%
    \fi%
    {\xtwo#2\relax#4\relax}%
  }%

  \catcode`\|=12
\newcommand*{\dotwo}[2]{}


%%%%%%%%%%%%%% Title page %%%%%%%%%%%%%%%%%%%%%
\RequirePackage{calc}
\RequirePackage{pstricks}
\RequirePackage{hyphenat}

\renewcommand{\maketitle}{%
\sloppy
\noindent{\fontsize{14}{13.5}\fontseries{b}\selectfont\raggedright\nohyphens{\@title}}
\vspace{.5in}

\fussy
\renewcommand*{\dotwo}[2]{%
\noindent##1\\
\noindent\textit{##2}
\vspace{1em}\par
}
\looptwo\listauthors\listaffiliations

\vspace{1em}
\parbox{\textwidth-\parindent-\parindent}{\small
\textbf{Abstract}\\\show@abstract
\\\\
\textbf{Keywords:} \show@keywords
}
}

%%%%%%%%%%%%%% Sections %%%%%%%%%%%%%%%%%%%%%
\renewcommand{\section}{%
\@startsection{section}{1}{0pt}%
{-1.5ex plus -1ex minus -.2ex}{2ex}%
{\fontsize{14}{13.5}\fontseries{b}\selectfont}%
}
\setcounter{secnumdepth}{0}

\renewcommand{\appendixname}{Appendix}

% From: https://tex.stackexchange.com/a/160850
\def\@seccntformat#1{\@ifundefined{#1@cntformat}%
   {\csname the#1\endcsname\quad}  % default
   {\csname #1@cntformat\endcsname}% enable individual control
}
\let\oldappendix\appendix %% save current definition of \appendix
\renewcommand\appendix{%
    \oldappendix    
    % Force sections to start on new page
    \let\oldsection\section
    \renewcommand{\section}{\clearpage\oldsection}
    % Renew sections to 'Appendix A <title>'
    \setcounter{secnumdepth}{1} % start numbering sections again
    \newcommand{\section@cntformat}{\appendixname~\thesection\quad}
    % Count figures/tables from A1
    \setcounter{table}{0}
    \renewcommand{\thetable}{A\arabic{table}}
    \setcounter{figure}{0}
    \renewcommand{\thetable}{A\arabic{table}}
}
\makeatother


%%%%%%%%%%%%%% Reference handling %%%%%%%%%%%%%%%%%%%%%
\RequirePackage{csquotes}
\RequirePackage[style=apa,sortcites=true,sorting=nyt,backend=biber]{biblatex}

\DeclareLanguageMapping{american}{american-apa}

\renewcommand{\bibfont}{\small}
\setlength{\bibhang}{\parindent}
