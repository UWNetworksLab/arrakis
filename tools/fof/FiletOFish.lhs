%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

\documentclass{report}
\usepackage[english]{babel}
\usepackage{fullpage}

%include polycode.fmt

\usepackage{epigraph}
\setlength{\epigraphrule}{0pt}
\setlength{\beforeepigraphskip}{0pt}
\setlength{\afterepigraphskip}{2\baselineskip}

\usepackage{tikz}
\usetikzlibrary{matrix,arrows}


\title{\Huge{Filet-o-Fish}\\
      \large{When French Cuisine Meets Swiss Fishes}
}
\author{\Large{Pierre-Evariste \sc{Dagand}}}
\date{}

\begin{document}

\maketitle

\newpage


\tableofcontents

\input{Introduction}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\part{The Filet-o-Fish Language}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\input{IntroLanguage}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\chapter{Filet-o-Fish Syntax}
\label{chap:fof_syntax}
\epigraph{- None shall pass.\\
          - I have no quarrel with you, good Sir Knight, but I must cross this bridge.\\
          - Then you shall die.}{Monty Python}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\input{PureExpressions}
\input{Constructs}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\chapter{Filet-o-Fish Semantics}
\label{chap:fof_semantics}
\epigraph{So, logically...\\
          If...\\
          she...\\ weighs...\\
          the same as a duck,...\\
          she's made of wood.}{Monty Python}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\input{Eval}
\input{Expressions}
\input{Semantics}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\chapter{Filet-o-Fish Operators}
\label{chap:fof_operators}
\epigraph{Listen. \\
          Strange women lying in ponds distributing swords 
          is no basis for a system of government. 
          Supreme executive power derives from a mandate from the masses, 
          not from some farcical aquatic ceremony.}{Monty Python}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\input{./Constructs/Arrays}
\input{./Constructs/Conditionals}
\input{./Constructs/Enumerations}
\input{./Constructs/Functions}
\input{./Constructs/References}
\input{./Constructs/Strings}
\input{./Constructs/Structures}
\input{./Constructs/Typedef}
\input{./Constructs/Unions}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\chapter{Lib-C Operators}
\label{chap:fof_libc}
\epigraph{Mortician: Bring out your dead! [clang] \ldots \\
          Customer:  Here's one -- nine pence. \\
          Dead person: I'm not dead!\\
          Mortician: What?\\
          Customer: Nothing -- here's your nine pence.}{Monty Python}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\input{./Libc/Printf}
\input{./Libc/Assert}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\chapter{Lib-barrefish Operators}
\label{chap:fof_libbarrelfish}
\epigraph{Here may be found the last words of Joseph of Aramathea.  He
  who is valiant and pure of spirit may find the Holy Grail in the
  Castle of uuggggggh}{Monty Python}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\input{./Libbarrelfish/HasDescendants}
\input{./Libbarrelfish/MemToPhys}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\part{The Filet-o-Fish Compiler}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\input{Compile}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\chapter{The FoF Intermediate Language}
\label{chap:il_fof}
\epigraph{- [...] For, since the tragic death of her father -- \\
          - He's not quite dead!\\
          - Since the near fatal wounding of her father--\\
          - He's getting better!\\
          - For, since her own father\ldots who, when he seemed about to
            recover, suddenly felt the icy hand of death upon him,\ldots\\
          - Oh, he's died!\\
          - And I want his only daughter to look upon me\ldots as her own
            dad -- in a very real, and legally binding sense.
            And I feel sure that the merger -- uh, the union -- between the
            Princess and the brave, but dangerous, Sir Launcelot of Camelot...}{Monty Python}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\input{./IL/FoF/FoF}
\input{./IL/FoF/Compile}
\input{./IL/FoF/Run}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\chapter{The Paka Intermediate Language}
\label{chap:il_paka}
\epigraph{Listen, lad. \\
  I've built this kingdom up from nothing.  When
  I started here, all there was was swamp.  All the kings said I was
  daft to build a castle in a swamp, but I built it all the same,
  just to show 'em.  It sank into the swamp.\\
  So, I built a second one. That sank into the swamp. \\
  So I built a third one.  That burned down,
  fell over, then sank into the swamp.  \\
  But the fourth one stayed up. An' that's what your gonna get, lad -- the strongest castle in these
  islands.}{Monty Python}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\input{./IL/Paka/Syntax}
\input{./IL/Paka/Builders}
\input{./IL/Paka/Paka}
\input{./IL/Paka/Compile}
\input{./IL/Paka/Optimizer}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\appendix
\part{Appendix}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\input{FutureWork}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\bibliographystyle{plain}
\bibliography{FiletOFish}


\end{document}
