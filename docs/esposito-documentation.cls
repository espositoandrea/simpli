% vim: ft=tex :

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{esposito-documentation}

\LoadClass[a4paper, 11pt, openright]{book}

\RequirePackage{ifthen}
\newboolean{isdraft}
\DeclareOption{draft}{\setboolean{isdraft}{true}}
\ProcessOptions\relax

\RequirePackage[inner=3.5cm,outer=3cm,top=2.5cm,bottom=2.5cm]{geometry}
\RequirePackage{hyperref}
\RequirePackage{float}
\RequirePackage{mathtools}
\RequirePackage{tikz}
\usetikzlibrary{angles}
\RequirePackage{amsmath}
\RequirePackage{blkarray}
\RequirePackage[htt]{hyphenat}
\RequirePackage{amssymb}
\RequirePackage[framemethod=tikz]{mdframed}
\RequirePackage{amsthm}
\RequirePackage{palatino}
\RequirePackage{listings}
\RequirePackage{xcolor}
\RequirePackage[english]{babel}
\RequirePackage{cancel}
\RequirePackage{booktabs}
\RequirePackage{longtable}
\RequirePackage{makecell}
\RequirePackage{graphicx}
\RequirePackage{titlesec}
\RequirePackage{syntax}

\renewcommand{\syntleft}{$\langle$\normalfont}
\renewcommand{\syntright}{$\rangle$}
\setlength{\grammarindent}{6em}

\ifthenelse{\boolean{isdraft}}{%
	\RequirePackage{draftwatermark}%
}{}

\renewcommand\theadfont{\bfseries}

\titleformat{\chapter}[hang]
	{\normalfont\bfseries\huge\raggedright}
	{\thechapter. }
	{15pt}
	{}
\titlespacing*{\chapter}{0pt}{-5ex}{3ex}


\setcounter{tocdepth}{3}
\setcounter{secnumdepth}{3}

\RequirePackage{fancyhdr}
\fancypagestyle{plain}{%
	\fancyhf{}
	\renewcommand{\headrulewidth}{0pt}
	\fancyfoot[LE,RO]{\thepage}
}
\pagestyle{fancy}
\fancyhf{}
\fancyfoot[LE,RO]{\thepage}
\fancyhead[LE,RO]{\leftmark}

% Edit the \maketitle command so that it does not remove data from \title and
% \author
\let\m@ketitle=\maketitle
\def\maketitle{%
\let\@@author=\@author\let\@@title=\@title\let\@@date=\@date%
\m@ketitle%
\let\author=\@@author\let\title=\@@title\let\date=\@@date%
}

\def\project#1{\gdef\@project{#1}}
\def\@project{\@latex@warning@no@line{No \noexpand\project given}}
\def\subtitle#1{\gdef\@subtitle{#1}}
\def\@subtitle{\@latex@warning@no@line{No \noexpand\subtitle given}}
\def\studentid#1{\gdef\@studentid{#1}}
\def\@studentid{\@latex@warning@no@line{No \noexpand\studentid given}}
\def\professor#1{\gdef\@professor{#1}}
\def\@professor{\@latex@warning@no@line{No \noexpand\professor given}}

\renewcommand\maketitle{%
	\begin{titlepage}
		\thispagestyle{empty}
		\center
		 
		\includegraphics[width=0.6\textwidth]{uniba}\\[0.25cm]
		\textsc{\LARGE University of Bari}\\
		\textsc{Department of Computer Science}\\[1.5cm]
		\textsc{Master's Degree in Computer Science}\\
		\textsc{\small (Curriculum: Artificial Intelligence)}\\[2.5cm]

		\textsc{\Large \@project}\\[0.5cm]

		\rule{\linewidth}{1pt}\\[0.4cm]
		\textsc{\huge \bfseries \@title}\\[3pt]
		\textsc{\LARGE \bfseries \@subtitle}\\[0.2cm]
		 \rule{\linewidth}{1pt}\\[1.5cm]
		 
		\begin{minipage}{0.4\textwidth}
			\begin{flushleft}
				\large
				\emph{Author:}\\
				\@author\ {\small (ID: \@studentid)} \\
			\end{flushleft}
		\end{minipage}
		~
		\begin{minipage}{0.4\textwidth}
			\begin{flushright}
				\large
				\emph{Professor:} \\
				\@professor
			\end{flushright}
		\end{minipage}\\[2cm]

		\vfill
		\rule{\linewidth}{1pt}\\[1pt]
		{\large Academic Year \@date}
		\clearpage
		\thispagestyle{empty}
	\end{titlepage}
}

\newcommand{\colophon}{%
	\vspace*{\fill}
	\begingroup
	\small
	\noindent
	\textbf{\title}\\
	\footnotesize
	Copyright \copyright\ 2020, \@author.\\
	Typeset using \LaTeX, written using Vim and Vim-\LaTeX. Compiled using
	\TeX\ Live.\\[1em]
	\textbf{Course:} \@project.\\
	\textbf{Degree Course:} Master's Degree in Computer Science (curriculum:
	``Artificial Intelligence'').\\
	\textbf{Academic year:} \@date.
	\endgroup
	\cleardoublepage
}

% Math commands
\newcommand{\R}{\mathbb{R}}
\newcommand{\N}{\mathbb{N}}
\newcommand{\C}{\mathbb{C}}
\newcommand{\F}{\mathbb{F}}
\newcommand{\f}[2][f]{#1\left(#2\right)}

\lstset{
	numbers=left,
	basicstyle=\ttfamily\footnotesize,
	breaklines=true,
	postbreak=\mbox{$\hookrightarrow$\space},
	numberstyle=\tiny,
	tabsize=4,
	backgroundcolor=\color[HTML]{f6f6f6},
	xleftmargin=2em,
	framexleftmargin=0.75em,
}
