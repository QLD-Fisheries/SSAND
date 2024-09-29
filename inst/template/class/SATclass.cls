% Copyright 2024 Fisheries Queensland

% This file is part of SSAND.
% SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
% SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
% You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.


\LoadClass[10pt]{report}
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{class/SATclass}[ ]

\RequirePackage{class/SATmacros} % title page and copyright info
\RequirePackage[a4paper, top=2.5cm,bottom=2.5cm,left=2.5cm,right=2.5cm]{geometry}
\RequirePackage{soul}
\RequirePackage{longtable}
\RequirePackage{multirow}
\RequirePackage{tabularx}
\RequirePackage[nottoc,notlof,notlot]{tocbibind}
\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{siunitx}
\RequirePackage{wrapfig}
\RequirePackage{float}
\RequirePackage[section]{placeins}
\RequirePackage{titling}
\RequirePackage[compact]{titlesec} % allows for definition of heading styles
\RequirePackage[titles]{tocloft} % line spacing in TOC
\RequirePackage[table]{xcolor} % for stripey tables
\RequirePackage{pxfonts} % uses Palatino/Pxfonts font family
\RequirePackage[onehalfspacing]{setspace} % looser line spacing
\RequirePackage{fancyhdr} % fancy header/footer
\RequirePackage{enumitem}
\RequirePackage{bm} % allows for bold equations
\RequirePackage{etoolbox}
\RequirePackage{graphicx}
\RequirePackage[defaultlines=2,all]{nowidow} % avoid page breaks mid-paragraphs
\RequirePackage[utf8]{inputenc}
\RequirePackage[backend=biber,uniquename=false,maxbibnames=999,uniquelist=false,mincitenames=1,maxcitenames=2,citestyle=authoryear,bibstyle=authoryear,sorting=nyt,doi=false,isbn=false,url=false]{biblatex}
\RequirePackage{pdfcomment}
\RequirePackage{threeparttable}
\RequirePackage{booktabs}
\RequirePackage{datetime}

\hypersetup{hidelinks}

\DeclareSourcemap{\maps[datatype=bibtex]{\map{\step[typesource=techreport, typetarget=report]}}}


\newcolumntype{L}[1]{>{\raggedright\let\newline\\\arraybackslash\hspace{0pt}}m{#1}}
\newcolumntype{C}[1]{>{\centering\let\newline\\\arraybackslash\hspace{0pt}}m{#1}}

% -------------------------------------------------------------------------
% Colours:
\definecolor{heading-gray}{RGB}{89,89,89} % same ugly gray as template headings
\definecolor{light-gray}{gray}{0.9}       % table alt rows

% Tables:
\setlength{\arrayrulewidth}{0.8pt} % default is 0.4 which disappears at some zoom levels
\setlength\extrarowheight{2pt}     % extra height in rows to make them less cramped

% Headers/Footers:
\pagestyle{fancyplain}
\fancyhf{}                         % clear header and footer
\renewcommand{\headrulewidth}{0pt} % removes header ruled line
\rfoot{\small \fancyplain{}{\thepage} }  % set right side of footer
\lfoot{\small \fancyplain{}{\DAFfooter}} %  set left side of footer

% Fonts:
\renewcommand{\rmdefault}{phv} % Changes body to Helvetica
\newcommand{\fignote}[1]{\textcolor{heading-gray}{\small #1}} % define style for figure/table notes

% Headings:
\def\chapsize{\fontsize{15}{0}\selectfont}         % define heading size
\def\secsize{\fontsize{13.5}{12}\selectfont}       % define heading size
\def\subsecsize{\fontsize{12.5}{12}\selectfont}    % define heading size
\def\subsubsecsize{\fontsize{11.5}{12}\selectfont} % define heading size

\titleformat{\chapter}{\normalfont\LARGE\bfseries}{\thechapter}{1em}{}                % fixes chapter number layout
\titleformat*{\section}{\normalfont\chapsize\bfseries\color{heading-gray}}            % fixes chapter number layout
\titleformat*{\subsection}{\normalfont\subsecsize\bfseries}                           % fixes chapter number layout
\titleformat*{\subsubsection}{\normalfont\subsubsecsize\bfseries\color{heading-gray}} % fixes chapter number layout

\titlespacing*{\chapter}{0pt}{-10pt}{10pt}      % adjusts white space around heading
\titlespacing*{\section} {0pt}{0pt}{0pt}        % adjusts white space around heading
\titlespacing*{\subsection} {0pt}{0pt}{0pt}     % adjusts white space around heading
\titlespacing*{\subsubsection}{0pt}{0pt}{0pt}   % adjusts white space around heading

% Captions:
\usepackage{caption}
\captionsetup[figure]{font={color=heading-gray},labelfont=bf}
\captionsetup[table]{font={color=heading-gray},labelfont=bf}
\captionsetup{justification=justified,singlelinecheck=false}

% TOC and Heading Numbering:
\setcounter{secnumdepth}{3} % includes numbering for subsubsection
\setcounter{tocdepth}{4}    % number of levels included in TOC
\setlength{\cftbeforechapskip}{3pt} % line spacing in TOC

% Bullet lists
\setlist{nolistsep} % no gap between paragraph and bullets

% Equation spacing
\AtBeginDocument{%
\setlength{\abovedisplayskip}{-10pt}
\setlength{\belowdisplayskip}{0pt}
\setlength{\abovedisplayshortskip}{-10pt}
\setlength{\belowdisplayshortskip}{0pt}
}

% Allow tex to still compile if image files are missing
% Requires packages: graphicx and etoolbox
\newcommand{\noimage}{%
  \setlength{\fboxsep}{-\fboxrule}%
  \fbox{\phantom{\rule{150pt}{100pt}}}% Framed box
}
\makeatletter
\patchcmd{\Gin@ii}
  {\begingroup}% <search>
  {\begingroup\renewcommand{\@latex@error}[2]{\noimage}}% <replace>
  {}% <success>
  {}% <failure>
\makeatother
% -------------------------------------------------------------------------
