% Copyright 2024 Fisheries Queensland

% This file is part of SSAND.
% SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
% SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
% You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.


\LoadClass{beamer}
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{class/SATpresentation}

\usetheme{madrid}
\usecolortheme{dove}
\usefonttheme{default} % structurebold
\setbeamertemplate{caption}[numbered] % number figures
\setbeamertemplate{navigation symbols}{} % remove navigation line
\setbeamertemplate{itemize items}[circle] % bullet points

%% Footer
\setbeamertemplate{footline}
{
  \leavevmode%
  \hbox{%
  \begin{beamercolorbox}[wd=.333333\paperwidth,ht=2.25ex,dp=1ex,center]{author in head/foot}%
    \usebeamerfont{author in head/foot}{Meeting \meetingnumber}
  \end{beamercolorbox}%
  \begin{beamercolorbox}[wd=.333333\paperwidth,ht=2.25ex,dp=1ex,center]{title in head/foot}%
    \usebeamerfont{title in head/foot}{\speciesname{} Project Team Meeting}
  \end{beamercolorbox}%
  \begin{beamercolorbox}[wd=.333333\paperwidth,ht=2.25ex,dp=1ex,right]{date in head/foot}%
    \usebeamerfont{date in head/foot}\insertframenumber{} / \inserttotalframenumber\hspace*{2ex}
  \end{beamercolorbox}}%
  \vskip0pt%
}
\makeatother


%% Set title page
\defbeamertemplate*{title page}{customized}[1][]
{
  \centering
  \usebeamerfont{title}{\speciesname} \\
  \usebeamerfont{subtitle}{Stock Assessment Project Team} \\
  \usebeamerfont{subtitle}{Meeting \meetingnumber} \\
  \vspace{3em}
  \includegraphics[width=0.4\textwidth]{\fishimage} \\
}
