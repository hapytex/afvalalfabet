\ProvidesClass{dictionary}
\LoadClassWithOptions{article}
% \RequirePackage{makeidx,amsmath,amsthm,amssymb,amsfonts,glossaries,dictionary,xparse,hyperref}
\RequirePackage{dictionary, makeidx, glossaries}
\RequirePackage[a5paper, top=2cm, headsep=0.4cm, bottom=1.5cm, left=1.5cm, right=1.5cm, landscape]{geometry}
\makeindex
\makeglossaries

\newglossarystyle{dicgls}{%
 \setglossarystyle{treegroup}%
 \renewcommand*{\glossentry}[2]{%
   \glsentryitem{##1}\entry[##1]{\glstarget{##1}{\glossentryname{##1}}}{\glossentrydesc{##1}}
   %\glsentryitem{##1}\textit{\glstarget{##1}{\glossentryname{##1}}}%
   %:\space \glossentrydesc{##1}\par
   %\entry[\glsentryitem{##1}]{\glstarget{##1}}{\glossentrydesc{##1}}
 }%
 \renewcommand*{\glsgroupheading}[1]{\ifthenelse{\equal{#1}{glssymbols}}{\clearpage\lettergroup{\#}\dictchar{\#}}{\clearpage\lettergroup{\glsgetgrouptitle{##1}}\dictchar{\glsgetgrouptitle{##1} \lowercase{\glsgetgrouptitle{##1}}}}}
 \renewcommand*{\glossarysection}[2][]{}
}
\setglossarystyle{dicgls}

\AtBeginDocument{\begin{titlepage}\maketitle\end{titlepage}\begin{twocolumn}\pagestyle{dict}}
\AtEndDocument{\end{twocolumn}}
