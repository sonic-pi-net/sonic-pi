#!/bin/sed -nf
# sokoban.sed  <http://aurelio.net/sed/sokoban>
#   by Aurelio Marinho Jargas <verde (a) aurelio net>
#
# Motivated by reading the amazing Adventure (Colossal Cave) history
#      <http://www.rickadams.org/adventure/a_history.html>
# GPL levels took from Mike Sharpe's sokoban.vim <http://vim.sourceforge.net>
#
# IMPORTANT
# This script has terminal control chars, so you must DOWNLOAD this
# file. Just copy/paste or printing it to a file (lynx) will NOT work.
#
# THE GAME
# You know sokoban. Everybody knows sokoban.
# Right, if you don't, it's a box pushing game. You have a mess, boxes
# all over the room and must place them on the boxes place. To move a
# box you must push it. You can only push a box if the path is clear
# with no wall or other box on the way (You are not that strong).
#
#   COMMANDS                       MOVING AROUND
#     :q   quit                      h or <left-arrow>  - move left
#     :r   restart level             j or <down-arrow>  - move down
#     :z   refresh screen            k or <up-arrow>    - move up
#     :gN  go to level N             l or <right-arrow> - move right
#
#   ACTORS
#     o box                  % wall
#     . box place            @ you
#     O box placed right     ! you over a box place
#
#
# RUNNING
# prompt$ ./sokoban.sed <enter>
# <enter>
# 1
# Now just move! (:q quits)
#
# DETAILS
# It's all written in SED, so we've got some limitations:
# - As a line-oriented editor, you MUST hit <ENTER> after *any* move.
#   Yes, that sucks. But you can accumulate various movements and hit
#   <ENTER> just once.
# - When you run sokoban.sed, you must first press any key to feed SED
#   and then you'll see the welcome message.
# - If your sed is not on /bin, edit the first line of this script,
#   or call it as: sed -nf sokoban.sed 
# - This script is 'sedcheck' <http://lvogel.free.fr/sed/sedcheck.sed>
#   compliant, so it must run fine in *any* SED version
#
# And always remember, it's cool because it's SED. If you don't like it
# you can try xsokoban instead <http://www.cs.cornell.edu/andru/xsokoban.html>
#
# CHANGES
# 20020315 v0.0 debut release
# 20020321 v0.1 clear screen, download note, fancy victory, sound ^G, lvl0
#               fixed * on map bug, added :g, :r and :z commands
#               pseudo-functions (now it's faster!)
# 20020709 v0.2 comments prefix '#r' changed to plain '#'. dummy me.
# 20031011 v0.3 now sokoban is 'sedcheck' compliant, so it will run in
#               most (all?) SED versions out there (thanks Laurent!)


# skip functions
b zero

# function welcome
: welcome
i \
       Welcome to the SED Sokoban\
\
Please select a level to begin [1-90]:
d  

# function loadmap
: loadmap

# clear screen
i \
[2J

/^0$/ {
  s/.*/\
SED Sokoban - LEVEL 0 (victory test)\
\
     %%%%%            \
     %@o.%            \
     %%%%%            \
/
  b endmap
}  
/^1$/ {
  s/.*/\
SED Sokoban - LEVEL 1\
\
     %%%%%            \
     %   %            \
     %o  %            \
   %%%  o%%           \
   %  o o %           \
 %%% % %% %   %%%%%%  \
 %   % %% %%%%%  ..%  \
 % o  o          ..%  \
 %%%%% %%% %@%%  ..%  \
     %     %%%%%%%%%  \
     %%%%%%%          \
/
  b endmap
}  
/^2$/ {
  s/.*/\
SED Sokoban - LEVEL 2\
\
 %%%%%%%%%%%%         \
 %..  %     %%%       \
 %..  % o  o  %       \
 %..  %o%%%%  %       \
 %..    @ %%  %       \
 %..  % %  o %%       \
 %%%%%% %%o o %       \
   % o  o o o %       \
   %    %     %       \
   %%%%%%%%%%%%       \
/
  b endmap
}  
/^3$/ {
  s/.*/\
SED Sokoban - LEVEL 3\
\
         %%%%%%%%     \
         %     @%     \
         % o%o %%     \
         % o  o%      \
         %%o o %      \
 %%%%%%%%% o % %%%    \
 %....  %% o  o  %    \
 %%...    o  o   %    \
 %....  %%%%%%%%%%    \
 %%%%%%%%             \
/
  b endmap
}  
/^4$/ {
  s/.*/\
SED Sokoban - LEVEL 4\
\
            %%%%%%%%  \
            %  ....%  \
 %%%%%%%%%%%%  ....%  \
 %    %  o o   ....%  \
 % ooo%o  o %  ....%  \
 %  o     o %  ....%  \
 % oo %o o o%%%%%%%%  \
 %  o %     %         \
 %% %%%%%%%%%         \
 %    %    %%         \
 %     o   %%         \
 %  oo%oo  @%         \
 %    %    %%         \
 %%%%%%%%%%%          \
/
  b endmap
}  
/^5$/ {
  s/.*/\
SED Sokoban - LEVEL 5\
\
         %%%%%        \
         %   %%%%%    \
         % %o%%  %    \
         %     o %    \
 %%%%%%%%% %%%   %    \
 %....  %% o  o%%%    \
 %....    o oo %%     \
 %....  %%o  o @%     \
 %%%%%%%%%  o  %%     \
         % o o  %     \
         %%% %% %     \
           %    %     \
           %%%%%%     \
/
  b endmap
}  
/^6$/ {
  s/.*/\
SED Sokoban - LEVEL 6\
\
 %%%%%%  %%%          \
 %..  % %%@%%         \
 %..  %%%   %         \
 %..     oo %         \
 %..  % % o %         \
 %..%%% % o %         \
 %%%% o %o  %         \
    %  o% o %         \
    % o  o  %         \
    %  %%   %         \
    %%%%%%%%%         \
/
  b endmap
}  
/^7$/ {
  s/.*/\
SED Sokoban - LEVEL 7\
\
        %%%%%         \
  %%%%%%%   %%        \
 %% % @%% oo %        \
 %    o      %        \
 %  o  %%%   %        \
 %%% %%%%%o%%%        \
 % o  %%% ..%         \
 % o o o ...%         \
 %    %%%...%         \
 % oo % %...%         \
 %  %%% %%%%%         \
 %%%%                 \
/
  b endmap
}  
/^8$/ {
  s/.*/\
SED Sokoban - LEVEL 8\
\
   %%%%               \
   %  %%%%%%%%%%%     \
   %    o   o o %     \
   % o% o %  o  %     \
   %  o o  %    %     \
 %%% o% %  %%%% %     \
 %@%o o o  %%   %     \
 %    o %o%   % %     \
 %   o    o o o %     \
 %%%%%  %%%%%%%%%     \
   %      %           \
   %      %           \
   %......%           \
   %......%           \
   %......%           \
   %%%%%%%%           \
/
  b endmap
}  
/^9$/ {
  s/.*/\
SED Sokoban - LEVEL 9\
\
           %%%%%%%    \
           %  ...%    \
       %%%%%  ...%    \
       %      . .%    \
       %  %%  ...%    \
       %% %%  ...%    \
      %%% %%%%%%%%    \
      % ooo %%        \
  %%%%%  o o %%%%%    \
 %%   %o o   %   %    \
 %@ o  o    o  o %    \
 %%%%%% oo o %%%%%    \
      %      %        \
      %%%%%%%%        \
/
  b endmap
}  
/^10$/ {
  s/.*/\
SED Sokoban - LEVEL 10\
\
  %%%  %%%%%%%%%%%%%  \
 %%@%%%%       %   %  \
 % oo   oo  o o ...%  \
 %  ooo%    o  %...%  \
 % o   % oo oo %...%  \
 %%%   %  o    %...%  \
 %     % o o o %...%  \
 %    %%%%%% %%%...%  \
 %% %  %  o o  %...%  \
 %  %% % oo o o%%..%  \
 % ..% %  o      %.%  \
 % ..% % ooo ooo %.%  \
 %%%%% %       % %.%  \
     % %%%%%%%%% %.%  \
     %           %.%  \
     %%%%%%%%%%%%%%%  \
/
  b endmap
}  
/^11$/ {
  s/.*/\
SED Sokoban - LEVEL 11\
\
           %%%%       \
      %%%% %  %       \
    %%% @%%%o %       \
   %%      o  %       \
  %%  o oo%% %%       \
  %  %o%%     %       \
  % % o oo % %%%      \
  %   o %  % o %%%%%  \
 %%%%    %  oo %   %  \
 %%%% %% o         %  \
 %.    %%%  %%%%%%%%  \
 %.. ..% %%%%         \
 %...%.%              \
 %.....%              \
 %%%%%%%              \
/
  b endmap
}  
/^12$/ {
  s/.*/\
SED Sokoban - LEVEL 12\
\
 %%%%%%%%%%%%%%%%     \
 %              %     \
 % % %%%%%%     %     \
 % %  o o o o%  %     \
 % %   o@o   %% %%    \
 % %  o o o%%%...%    \
 % %   o o  %%...%    \
 % %%%ooo o %%...%    \
 %     % %% %%...%    \
 %%%%%   %% %%...%    \
     %%%%%     %%%    \
         %     %      \
         %%%%%%%      \
/
  b endmap
}  
/^13$/ {
  s/.*/\
SED Sokoban - LEVEL 13\
\
    %%%%%%%%%         \
   %%   %%  %%%%%     \
 %%%     %  %    %%%  \
 %  o %o %  %  ... %  \
 % % o%@o%% % %.%. %  \
 %  % %o  %    . . %  \
 % o    o % % %.%. %  \
 %   %%  %%o o . . %  \
 % o %   %  %o%.%. %  \
 %% o  o   o  o... %  \
  %o %%%%%%    %%  %  \
  %  %    %%%%%%%%%%  \
  %%%%                \
/
  b endmap
}  
/^14$/ {
  s/.*/\
SED Sokoban - LEVEL 14\
\
        %%%%%%%       \
  %%%%%%%     %       \
  %     % o@o %       \
  %oo %   %%%%%%%%%   \
  % %%%......%%   %   \
  %   o......%% % %   \
  % %%%......     %   \
 %%   %%%% %%% %o%%   \
 %  %o   %  o  % %    \
 %  o ooo  % o%% %    \
 %   o o %%%oo % %    \
 %%%%%     o   % %    \
     %%% %%%   % %    \
       %     %   %    \
       %%%%%%%%  %    \
              %%%%    \
/
  b endmap
}  
/^15$/ {
  s/.*/\
SED Sokoban - LEVEL 15\
\
    %%%%%%%%          \
    %   %  %          \
    %  o   %          \
  %%% %o   %%%%       \
  %  o  %%o   %       \
  %  % @ o % o%       \
  %  %      o %%%%    \
  %% %%%%o%%     %    \
  % o%.....% %   %    \
  %  o..OO. o% %%%    \
 %%  %.....%   %      \
 %   %%% %%%%%%%      \
 % oo  %  %           \
 %  %     %           \
 %%%%%%   %           \
      %%%%%           \
/
  b endmap
}  
/^16$/ {
  s/.*/\
SED Sokoban - LEVEL 16\
\
 %%%%%                \
 %   %%               \
 %    %  %%%%         \
 % o  %%%%  %         \
 %  oo o   o%         \
 %%%@ %o    %%        \
  %  %%  o o %%       \
  % o  %% %% .%       \
  %  %o%%o  %.%       \
  %%%   o..%%.%       \
   %    %.O...%       \
   % oo %.....%       \
   %  %%%%%%%%%       \
   %  %               \
   %%%%               \
/
  b endmap
}  
/^17$/ {
  s/.*/\
SED Sokoban - LEVEL 17\
\
    %%%%%%%%%%        \
    %..  %   %        \
    %..      %        \
    %..  %  %%%%      \
   %%%%%%%  %  %%     \
   %            %     \
   %  %  %%  %  %     \
 %%%% %%  %%%% %%     \
 %  o  %%%%% %  %     \
 % % o  o  % o  %     \
 % @o  o   %   %%     \
 %%%% %% %%%%%%%      \
    %    %            \
    %%%%%%            \
/
  b endmap
}  
/^18$/ {
  s/.*/\
SED Sokoban - LEVEL 18\
\
      %%%%%%%%%%%     \
      %  .  %   %     \
      % %.    @ %     \
  %%%%% %%..% %%%%    \
 %%  % ..%%%     %%%  \
 % o %...   o %  o %  \
 %    .. %%  %% %% %  \
 %%%%o%%o% o %   % %  \
   %% %    %o oo % %  \
   %  o % %  % o%% %  \
   %               %  \
   %  %%%%%%%%%%%  %  \
   %%%%         %%%%  \
/
  b endmap
}  
/^19$/ {
  s/.*/\
SED Sokoban - LEVEL 19\
\
   %%%%%%             \
   %   @%%%%          \
 %%%%% o   %          \
 %   %%    %%%%       \
 % o %  %%    %       \
 % o %  %%%%% %       \
 %% o  o    % %       \
 %% o o %%% % %       \
 %% %  o  % % %       \
 %% % %o%   % %       \
 %% %%%   % % %%%%%%  \
 %  o  %%%% % %....%  \
 %    o    o   ..%.%  \
 %%%%o  o% o   ....%  \
 %       %  %% ....%  \
 %%%%%%%%%%%%%%%%%%%  \
/
  b endmap
}  
/^20$/ {
  s/.*/\
SED Sokoban - LEVEL 20\
\
     %%%%%%%%%%       \
 %%%%%        %%%%    \
 %     %   o  %@ %    \
 % %%%%%%%o%%%%  %%%  \
 % %    %% %  %o ..%  \
 % % o     %  %  %.%  \
 % % o  %     %o ..%  \
 % %  %%% %%     %.%  \
 % %%%  %  %  %o ..%  \
 % %    %  %%%%  %.%  \
 % %o   o  o  %o ..%  \
 %    o % o o %  %.%  \
 %%%% o%%%    %o ..%  \
    %    oo %%%....%  \
    %      %% %%%%%%  \
    %%%%%%%%          \
/
  b endmap
}  
/^21$/ {
  s/.*/\
SED Sokoban - LEVEL 21\
\
 %%%%%%%%%            \
 %       %            \
 %       %%%%         \
 %% %%%% %  %         \
 %% %@%%    %         \
 % ooo o  oo%         \
 %  % %% o  %         \
 %  % %%  o %%%%      \
 %%%%  ooo o%  %      \
  %   %%   ....%      \
  % %   % %.. .%      \
  %   % % %%...%      \
  %%%%% o  %...%      \
      %%   %%%%%      \
       %%%%%          \
/
  b endmap
}  
/^22$/ {
  s/.*/\
SED Sokoban - LEVEL 22\
\
 %%%%%%     %%%%      \
 %    %%%%%%%  %%%%%  \
 %   o%  %  o  %   %  \
 %  o  o  o % o o  %  \
 %%o o   % @% o    %  \
 %  o %%%%%%%%%%% %%  \
 % %   %.......% o%   \
 % %%  % ......%  %   \
 % %   o........o %   \
 % % o %.... ..%  %   \
 %  o o%%%%o%%%% o%   \
 % o   %%% o   o  %%  \
 % o     o o  o    %  \
 %% %%%%%% o %%%%% %  \
 %         %       %  \
 %%%%%%%%%%%%%%%%%%%  \
/
  b endmap
}  
/^23$/ {
  s/.*/\
SED Sokoban - LEVEL 23\
\
     %%%%%%%          \
     %  %  %%%%       \
 %%%%% o%o %  %%      \
 %.. %  %  %   %      \
 %.. % o%o %  o%%%%   \
 %.  %     %o  %  %   \
 %..   o%  % o    %   \
 %..@%  %o %o  %  %   \
 %.. % o%     o%  %   \
 %.. %  %oo%o  %  %%  \
 %.. % o%  %  o%o  %  \
 %.. %  %  %   %   %  \
 %%. %%%%  %%%%%   %  \
  %%%%  %%%%   %%%%%  \
/
  b endmap
}  
/^24$/ {
  s/.*/\
SED Sokoban - LEVEL 24\
\
 %%%%%%%%%%%%%%%      \
 %..........  .%%%%   \
 %..........oo.%  %   \
 %%%%%%%%%%%o %   %%  \
 %      o  o     o %  \
 %% %%%%   %  o %  %  \
 %      %   %%  % %%  \
 %  o%  % %%  %%% %%  \
 % o %o%%%    %%% %%  \
 %%%  o %  %  %%% %%  \
 %%%    o %% %  % %%  \
  % o  %  o  o o   %  \
  %  o  o%ooo  %   %  \
  %  %  o      %%%%%  \
  % @%%  %  %  %      \
  %%%%%%%%%%%%%%      \
/
  b endmap
}  
/^25$/ {
  s/.*/\
SED Sokoban - LEVEL 25\
\
 %%%%                 \
 %  %%%%%%%%%%%%%%    \
 %  %   ..%......%    \
 %  % % %%%%% ...%    \
 %%o%    ........%    \
 %   %%o%%%%%%  %%%%  \
 % o %     %%%%%%@ %  \
 %%o % o   %%%%%%  %  \
 %  o %ooo%%       %  \
 %      %    %o%o%%%  \
 % %%%% %ooooo    %   \
 % %    o     %   %   \
 % %   %%        %%%  \
 % %%%%%%o%%%%%% o %  \
 %        %    %   %  \
 %%%%%%%%%%    %%%%%  \
/
  b endmap
}  
/^26$/ {
  s/.*/\
SED Sokoban - LEVEL 26\
\
  %%%%%%%             \
  %  %  %%%%%         \
 %%  %  %...%%%       \
 %  o%  %...  %       \
 % o %oo ...  %       \
 %  o%  %... .%       \
 %   % o%%%%%%%%      \
 %%o       o o %      \
 %%  %  oo %   %      \
  %%%%%%  %%oo@%      \
       %      %%      \
       %%%%%%%%       \
/
  b endmap
}  
/^27$/ {
  s/.*/\
SED Sokoban - LEVEL 27\
\
  %%%%%%%%%%%%%%%%%   \
  %...   %    %   %%  \
 %%.....  o%% % %o %  \
 %......%  o  %    %  \
 %......%  %  % %  %  \
 %%%%%%%%% o  o o  %  \
   %     %o%%o %%o%%  \
  %%   o    % o    %  \
  %  %% %%% %  %%o %  \
  % o oo     o  o  %  \
  % o    o%%o %%%%%%  \
  %%%%%%%  @ %%       \
        %%%%%%        \
/
  b endmap
}  
/^28$/ {
  s/.*/\
SED Sokoban - LEVEL 28\
\
          %%%%%       \
      %%%%%   %       \
     %% o  o  %%%%    \
 %%%%% o  o o %%.%    \
 %       oo  %%..%    \
 %  %%%%%% %%%.. %    \
 %% %  %    %... %    \
 % o   %    %... %    \
 %@ %o %% %%%%...%    \
 %%%%  o oo  %%..%    \
    %%  o o  o...%    \
     % oo  o %  .%    \
     %   o o  %%%%    \
     %%%%%%   %       \
          %%%%%       \
/
  b endmap
}  
/^29$/ {
  s/.*/\
SED Sokoban - LEVEL 29\
\
 %%%%%                \
 %   %%               \
 % o  %%%%%%%%%       \
 %% % %       %%%%%%  \
 %% %   o%o%@  %   %  \
 %  %      o %   o %  \
 %  %%% %%%%%%%%% %%  \
 %  %% ..O..... % %%  \
 %% %% O.O..O.O % %%  \
 % o%%%%%%%%%% %%o %  \
 %  o   o  o    o  %  \
 %  %   %   %   %  %  \
 %%%%%%%%%%%%%%%%%%%  \
/
  b endmap
}  
/^30$/ {
  s/.*/\
SED Sokoban - LEVEL 30\
\
        %%%%%%%%%%%   \
        %   %     %   \
 %%%%%  %     o o %   \
 %   %%%%% o%% % %%   \
 % o %%   % %% o  %   \
 % o  @oo % %%ooo %   \
 %% %%%   % %%    %   \
 %% %   %%% %%%%%o%   \
 %% %     o  %....%   \
 %  %%% %% o %....%%  \
 % o   o %   %..o. %  \
 %  %% o %  %%.... %  \
 %%%%%   %%%%%%...%%  \
     %%%%%    %%%%%   \
/
  b endmap
}  
/^31$/ {
  s/.*/\
SED Sokoban - LEVEL 31\
\
   %%%%               \
   %  %%%%%%%%%       \
  %%  %%  %   %       \
  %  o% o@o   %%%%    \
  %o  o  % o o%  %%   \
 %%  o%% %o o     %   \
 %  %  % %   ooo  %   \
 % o    o  o%% %%%%   \
 % o o %o%  %  %      \
 %%  %%%  %%%o %      \
  %  %....     %      \
  %%%%......%%%%      \
    %....%%%%         \
    %...%%            \
    %...%             \
    %%%%%             \
/
  b endmap
}  
/^32$/ {
  s/.*/\
SED Sokoban - LEVEL 32\
\
       %%%%           \
   %%%%%  %           \
  %%     o%           \
 %% o  %% %%%         \
 %@o o % o  %         \
 %%%% %%   o%         \
  %....%o o %         \
  %....%   o%         \
  %....  oo %%        \
  %... % o   %        \
  %%%%%%o o  %        \
       %   %%%        \
       %o %%%         \
       %  %           \
       %%%%           \
/
  b endmap
}  
/^33$/ {
  s/.*/\
SED Sokoban - LEVEL 33\
\
  %%%%%%%%%%%         \
  %     %%  %         \
  %   o   o %         \
 %%%% %% oo %         \
 %   o %    %         \
 % ooo % %%%%         \
 %   % % o %%         \
 %  %  %  o %         \
 % o% o%    %         \
 %   ..% %%%%         \
 %%%%.. o %@%         \
 %.....% o% %         \
 %%....%  o %         \
  %%..%%    %         \
   %%%%%%%%%%         \
/
  b endmap
}  
/^34$/ {
  s/.*/\
SED Sokoban - LEVEL 34\
\
  %%%%%%%%%           \
  %....   %%          \
  %.%.%  o %%         \
 %%....% % @%%        \
 % ....%  %  %%       \
 %     %o %%o %       \
 %% %%%  o    %       \
  %o  o o o%  %       \
  % %  o o %% %       \
  %  %%%  %%  %       \
  %    %% %% %%       \
  %  o %  o  %        \
  %%%o o   %%%        \
    %  %%%%%          \
    %%%%              \
/
  b endmap
}  
/^35$/ {
  s/.*/\
SED Sokoban - LEVEL 35\
\
 %%%%%%%%%%%% %%%%%%  \
 %   %    % %%%....%  \
 %   oo%   @  .....%  \
 %   % %%%   % ....%  \
 %% %% %%%  %  ....%  \
  % o o     % % %%%%  \
  %  o o%%  %      %  \
 %%%% %  %%%% % %% %  \
 %  % %o   %% %    %  \
 % o  o  % %% %   %%  \
 % % o o    % %   %   \
 %  o %% %% % %%%%%   \
 % oo     oo  %       \
 %% %% %%% o  %       \
  %    % %    %       \
  %%%%%% %%%%%%       \
/
  b endmap
}  
/^36$/ {
  s/.*/\
SED Sokoban - LEVEL 36\
\
             %%%%%    \
 %%%%%  %%%%%%   %    \
 %   %%%%  o o o %    \
 % o   %% %% %%  %%   \
 %   o o     o  o %   \
 %%% o  %% %%     %%  \
   % %%%%% %%%%%oo %  \
  %%o%%%%% @%%     %  \
  % o  %%%o%%% o  %%  \
  % o  %   %%%  %%%   \
  % oo o %   oo %     \
  %     %   %%  %     \
  %%%%%%%.. .%%%      \
     %.........%      \
     %.........%      \
     %%%%%%%%%%%      \
/
  b endmap
}  
/^37$/ {
  s/.*/\
SED Sokoban - LEVEL 37\
\
 %%%%%%%%%%%          \
 %......   %%%%%%%%%  \
 %......   %  %%   %  \
 %..%%% o    o     %  \
 %... o o %   %%   %  \
 %...%o%%%%%    %  %  \
 %%%    %   %o  %o %  \
   %  oo o o  o%%  %  \
   %  o   %o%o %%o %  \
   %%% %% %    %%  %  \
    %  o o %% %%%%%%  \
    %    o  o  %      \
    %%   % %   %      \
     %%%%%@%%%%%      \
         %%%          \
/
  b endmap
}  
/^38$/ {
  s/.*/\
SED Sokoban - LEVEL 38\
\
       %%%%           \
 %%%%%%% @%           \
 %     o  %           \
 %   o%% o%           \
 %%o%...% %           \
  % o...  %           \
  % %. .% %%          \
  %   % %o %          \
  %o  o    %          \
  %  %%%%%%%          \
  %%%%                \
/
  b endmap
}  
/^39$/ {
  s/.*/\
SED Sokoban - LEVEL 39\
\
              %%%%%%  \
  %%%%%%%%%%%%%....%  \
 %%   %%     %%....%  \
 %  oo%%  o @%%....%  \
 %      oo o%  ....%  \
 %  o %% oo % % ...%  \
 %  o %% o  %  ....%  \
 %% %%%%% %%% %%.%%%  \
 %%   o  o %%   .  %  \
 % o%%%  % %%%%% %%%  \
 %   o   %       %    \
 %  o %o o o%%%  %    \
 % ooo% o   % %%%%    \
 %    %  oo %         \
 %%%%%%   %%%         \
      %%%%%           \
/
  b endmap
}  
/^40$/ {
  s/.*/\
SED Sokoban - LEVEL 40\
\
     %%%%%%%%%%%%     \
     %          %%    \
     %  % %oo o  %    \
     %o %o%  %% @%    \
    %% %% % o % %%    \
    %   o %o  % %     \
    %   % o   % %     \
    %% o o   %% %     \
    %  %  %%  o %     \
    %    %% oo% %     \
 %%%%%%oo   %   %     \
 %....%  %%%%%%%%     \
 %.%... %%            \
 %....   %            \
 %....   %            \
 %%%%%%%%%            \
/
  b endmap
}  
/^41$/ {
  s/.*/\
SED Sokoban - LEVEL 41\
\
            %%%%%     \
           %%   %%    \
          %%     %    \
         %%  oo  %    \
        %% oo  o %    \
        % o    o %    \
 %%%%   %   oo %%%%%  \
 %  %%%%%%%% %%    %  \
 %.            ooo@%  \
 %.% %%%%%%% %%   %%  \
 %.% %%%%%%%. %o o%%  \
 %........... %    %  \
 %%%%%%%%%%%%%%  o %  \
              %%  %%  \
               %%%%   \
/
  b endmap
}  
/^42$/ {
  s/.*/\
SED Sokoban - LEVEL 42\
\
      %%%%%%%%        \
   %%%%      %%%%%%   \
   %    %% o o   @%   \
   % %% %%o%o o o%%   \
 %%% ......%  oo %%   \
 %   ......%  %   %   \
 % % ......%o  o  %   \
 % %o...... oo% o %   \
 %   %%% %%%o  o %%   \
 %%%  o  o  o  o %    \
   %  o  o  o  o %    \
   %%%%%%   %%%%%%    \
        %%%%%         \
/
  b endmap
}  
/^43$/ {
  s/.*/\
SED Sokoban - LEVEL 43\
\
         %%%%%%%      \
     %%%%%  %  %%%%   \
     %   %   o    %   \
  %%%% %oo %% %%  %   \
 %%      % %  %% %%%  \
 %  %%% o%o  o  o  %  \
 %...    % %%  %   %  \
 %...%    @ % %%% %%  \
 %...%  %%%  o  o  %  \
 %%%%%%%% %%   %   %  \
           %%%%%%%%%  \
/
  b endmap
}  
/^44$/ {
  s/.*/\
SED Sokoban - LEVEL 44\
\
  %%%%%               \
  %   %               \
  % % %%%%%%%         \
  %      o@%%%%%%     \
  % o %%o %%%   %     \
  % %%%% o    o %     \
  % %%%%% %  %o %%%%  \
 %%  %%%% %%o      %  \
 %  o%  o  % %% %% %  \
 %         % %...% %  \
 %%%%%%  %%%  ...  %  \
      %%%% % %...% %  \
           % %%% % %  \
           %       %  \
           %%%%%%%%%  \
/
  b endmap
}  
/^45$/ {
  s/.*/\
SED Sokoban - LEVEL 45\
\
 %%%%% %%%%           \
 %...% %  %%%%        \
 %...%%%  o  %        \
 %....%% o  o%%%      \
 %%....%%   o  %      \
 %%%... %% o o %      \
 % %%    %  o  %      \
 %  %% % %%% %%%%     \
 % o % %o  o    %     \
 %  o @ o    o  %     \
 %   % o oo o %%%     \
 %  %%%%%%  %%%       \
 % %%    %%%%         \
 %%%                  \
/
  b endmap
}  
/^46$/ {
  s/.*/\
SED Sokoban - LEVEL 46\
\
 %%%%%%%%%%           \
 %        %%%%        \
 % %%%%%% %  %%       \
 % % o o o  o %       \
 %       %o   %       \
 %%%o  oo%  %%%       \
   %  %% % o%%        \
   %%o%   o @%        \
    %  o o %%%        \
    % %   o  %        \
    % %%   % %        \
   %%  %%%%% %        \
   %         %        \
   %.......%%%        \
   %.......%          \
   %%%%%%%%%          \
/
  b endmap
}  
/^47$/ {
  s/.*/\
SED Sokoban - LEVEL 47\
\
          %%%%        \
  %%%%%%%%%  %%       \
 %%  o      o %%%%%   \
 %   %% %%   %%...%   \
 % %oo o oo%o%%...%   \
 % %   @   %   ...%   \
 %  o% %%%oo   ...%   \
 % o  oo  o %%....%   \
 %%%o       %%%%%%%   \
   %  %%%%%%%         \
   %%%%               \
/
  b endmap
}  
/^48$/ {
  s/.*/\
SED Sokoban - LEVEL 48\
\
   %%%%%%%%%          \
   %O.O%O.O%          \
   %.O.O.O.%          \
   %O.O.O.O%          \
   %.O.O.O.%          \
   %O.O.O.O%          \
   %%%   %%%          \
     %   %            \
 %%%%%% %%%%%%        \
 %           %        \
 % o o o o o %        \
 %% o o o o %%        \
  %o o o o o%         \
  %   o@o   %         \
  %  %%%%%  %         \
  %%%%   %%%%         \
/
  b endmap
}  
/^49$/ {
  s/.*/\
SED Sokoban - LEVEL 49\
\
        %%%%          \
        %  %%         \
        %   %%        \
        % oo %%       \
      %%%o  o %%      \
   %%%%    o   %      \
 %%%  % %%%%%  %      \
 %    % %....o %      \
 % %   o ....% %      \
 %  o % %.O..% %      \
 %%%  %%%% %%% %      \
   %%%% @o  %%o%%     \
      %%% o     %     \
        %  %%   %     \
        %%%%%%%%%     \
/
  b endmap
}  
/^50$/ {
  s/.*/\
SED Sokoban - LEVEL 50\
\
       %%%%%%%%%%%%   \
      %%..    %   %   \
     %%..O o    o %   \
    %%..O.% % % o%%   \
    %..O.% % % o  %   \
 %%%%...%  %    % %   \
 %  %% %          %   \
 % @o o %%%  %   %%   \
 % o   o   % %   %    \
 %%%oo   % % % % %    \
   %   o   % % %%%%%  \
   % o% %%%%%      %  \
   %o   %   %    % %  \
   %  %%%   %%     %  \
   %  %      %    %%  \
   %%%%      %%%%%%   \
/
  b endmap
}  
/^51$/ {
  s/.*/\
SED Sokoban - LEVEL 51\
\
  %%%%%%%%%           \
  %       %           \
  % o oo o%           \
 %%% %  o %           \
 %.%   oo %%          \
 %.%%%   o %          \
 %.%. o %% %%%%       \
 %...  o%% o  %       \
 %...o   o    %       \
 %..%%%o%%% %@%       \
 %..% %     %%%       \
 %%%% %%%%%%%         \
/
  b endmap
}  
/^52$/ {
  s/.*/\
SED Sokoban - LEVEL 52\
\
            %%%%%%%%  \
            %......%  \
    %%%%    %......%  \
    %  %%%%%%%%%...%  \
    % o   o    %...%  \
    %  % % % % %   %  \
 %%%%% % % %@% %   %  \
 %   % %%% %%% %% %%  \
 %    o % o o o % %   \
 % ooo  o   %     %   \
 %   % %%%o%%%o%% %   \
 %%% %  o   %     %   \
  %% o  % o o o %%%   \
  %  % %%% %%% %%     \
  % o          %      \
  %  %%%%%%%%%%%      \
  %%%%                \
/
  b endmap
}  
/^53$/ {
  s/.*/\
SED Sokoban - LEVEL 53\
\
 %%%%%%%%%%%%%%%%%%   \
 %                %%  \
 % o%   o %%  o    %  \
 %    o%%%    % oo %  \
 %.%%%     o o %%  %% \
 %...%  %  %    %o  % \
 %..%%oo%%%% o  %   % \
 %...%      o %%  %%% \
 %...o  %%%  %    % % \
 %%..  o%  %%   %%@ % \
  %%.%              % \
   %%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^54$/ {
  s/.*/\
SED Sokoban - LEVEL 54\
\
 %%%%%%%%%%%%%%%%%%%% \
 %   %    %   %   %@% \
 % o      o   o   % % \
 %% %%%..%% %%%     % \
 %   %....%o%  o%%% % \
 % o %....%  o  o o % \
 %   %....% % % o o % \
 %   %%..%%   %o%   % \
 %%o%%    %%  %  %o%% \
 %   o  o     %  %  % \
 %   %    %   %     % \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^55$/ {
  s/.*/\
SED Sokoban - LEVEL 55\
\
 %%%%%%%%%%%%%%%%%%%% \
 %    @%%      %   %% \
 %    %%    o    o %% \
 %  %%%....% % %  %%% \
 %   %....% % % o   % \
 %%% %...%  %       % \
 %%  %%.%     o   o % \
 %%  o o %%%  % % %%% \
 %% o       % % o   % \
 %%%% o  o% % % % o % \
 %%%%         % %  %% \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^56$/ {
  s/.*/\
SED Sokoban - LEVEL 56\
\
 %%%%%%%%%%%%%%%%%%%% \
 %  %  %%    %   @%%% \
 %%    o    % o%%%  % \
 %%o% o %%o% o o    % \
 %   o%    o      %%% \
 % %%   o %%%  %....% \
 % % o% % % % %....%% \
 %    o o %  %....%%% \
 %%o %%%  o %....%%%% \
 %  % o        %%%%%% \
 %      % %    %%%%%% \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^57$/ {
  s/.*/\
SED Sokoban - LEVEL 57\
\
 %%%%%%%%%%%%%%%%%%%% \
 %@     %%%   %  %  % \
 % % %  %  o  o     % \
 %%%%%     % o o%o% % \
 %.%..%    %%o o    % \
 %.....    o   %   %% \
 %.....    %%%o%%o%%% \
 %.%..%    o    %   % \
 %%%%%     %  %o  o % \
 %%%%%  %  o    o o % \
 %%%%%  %  %  %  %  % \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^58$/ {
  s/.*/\
SED Sokoban - LEVEL 58\
\
 %%%%%%%%%%%%%%%%%%%% \
 %%...   %% %    %  % \
 %....         o %% % \
 %....% % %o%%%o    % \
 %...%    %       % % \
 %%.%  %o %     o%% % \
 %  %  % o o %%%  o % \
 %     o  o %  % %% % \
 %% % %% %oo% o%  % % \
 %  %   o o %      %% \
 %    %     %  %   @% \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^59$/ {
  s/.*/\
SED Sokoban - LEVEL 59\
\
 %%%%%%%%%%%%%%%%%%%% \
 %   %  %@% %%  %%%%% \
 % % %  o    o  %%%%% \
 % %    %%%%%% o  %%% \
 %   %  %....%  oo  % \
 %%o%%o%%....%      % \
 %      %....%%o%%o%% \
 %  oo  %....%      % \
 % o  o  %  %  %%%  % \
 %%%%%  o   o    o  % \
 %%%%% %    %  %   %% \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^60$/ {
  s/.*/\
SED Sokoban - LEVEL 60\
\
 %%%%%%%%%%%%%%%%%%%% \
 % %     %          % \
 %       o  %% %%% %% \
 %%%%%  %%   o  o   % \
 %%..%%  % % o % %  % \
 %....  o     %%o% %% \
 %....  o%%%%%   %o%% \
 %%..% %  %   %  o  % \
 %%%.% %  o   o  % @% \
 %%  o  o %   %  %%%% \
 %%       %%%%%%%%%%% \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^61$/ {
  s/.*/\
SED Sokoban - LEVEL 61\
\
 %%%%%%%%%%%%%%%%%%%% \
 %     %%%..%%%     % \
 % oo  %%%..%%%  o@ % \
 %  % %%......%  o  % \
 %     %......%  o  % \
 %%%%  %%%..%%%%%%o % \
 %   ooo %..%    %  % \
 % o%   o  o  oo %o % \
 %  %  %% o  %%  %  % \
 % o    o %% o    o % \
 %  %  %%    %%  %  % \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^62$/ {
  s/.*/\
SED Sokoban - LEVEL 62\
\
 %%%%%%%%%%%%%%%%%%%% \
 %    %  % %  %  %  % \
 % @% % %% o   o   %% \
 %%%% %    %  % o   % \
 %    % %% %o %% %% % \
 %      o   o   o   % \
 %..%%%oo%% o%%o %% % \
 %..%.%  % o   o %  % \
 %....% oo   %%o %%%% \
 %....%  %%%%%      % \
 %...%%%        %%  % \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^63$/ {
  s/.*/\
SED Sokoban - LEVEL 63\
\
 %%%%%%%%%%%%%%%%%%%% \
 %....%       %  %  % \
 %....% % o  o      % \
 %.... %%  o% % o%o % \
 %...%   o   o%  o  % \
 %..%%%%  % o   oo  % \
 %      %%%% %%%% %%% \
 %        %   %     % \
 % %%   %   o % o o % \
 % %%    o %% o  o  % \
 %     @%     %   % % \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^64$/ {
  s/.*/\
SED Sokoban - LEVEL 64\
\
 %%%%%%%%%%%%%%%%%%%% \
 %....%%%           % \
 %....%%%%% %  %o% %% \
 %....%%%   %o  o   % \
 %....%%%    o  %oo%% \
 %%  %%%% o%  %o o  % \
 %%  %%%%  o  o  %  % \
 %@  %%%%o%%%o%% o  % \
 %%        %  %  o  % \
 %%   %%%  %  o  %%%% \
 %%%%%%%%  %  %     % \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^65$/ {
  s/.*/\
SED Sokoban - LEVEL 65\
\
 %%%%%%%%%%%%%%%%%%%% \
 %     %     @%...%%% \
 %     %      %%...%% \
 % % % %%o%% %% ....% \
 %   o %   ooo  ....% \
 %%%o%%% oo  %%% %%.% \
 %     o  %    % %%%% \
 %  o  %  %%%  % %  % \
 %% %o%%    o  oo   % \
 %   o %%   %  % %  % \
 %     %    %  %    % \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^66$/ {
  s/.*/\
SED Sokoban - LEVEL 66\
\
 %%%%%%%%%%%%%%%%%%%% \
 %     %  %...%@    % \
 % %       ....%    % \
 %  o  %   %....%   % \
 % %%o%%%% %%....%  % \
 % o   o  %  %...%  % \
 % oo %   %   % oo  % \
 %%%  ooo%   oo  o  % \
 % o  %  %    % o%  % \
 %   o%  %       o  % \
 %  %    %    %  %  % \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^67$/ {
  s/.*/\
SED Sokoban - LEVEL 67\
\
 %%%%%%%%%%%%%%%%%%%% \
 %%%%%@%%%.%%...%%  % \
 %%%%%o  ..%...%    % \
 %%%%    ......%  o % \
 %%%  o %.....%% % %% \
 %%  oo% %%%%%  o o % \
 %% o% o    %%  oo  % \
 %%  %  %    % o  o % \
 %%   oo %%% %o%%   % \
 %% o%      o o  o %% \
 %%%    %    %    %%% \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^68$/ {
  s/.*/\
SED Sokoban - LEVEL 68\
\
 %%%%%%%%%%%%%%%%%%%% \
 %@     %   %       % \
 %% %%% %%  %%%% % %% \
 %    % %  oo       % \
 %  % % % o % o %% %% \
 %     o %  %oo %   % \
 %  %%%  %      %% %% \
 %..%.% o %  o %    % \
 %..%.%  o % %% oo  % \
 %....%%   oo  o  % % \
 %.....%%        %  % \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^69$/ {
  s/.*/\
SED Sokoban - LEVEL 69\
\
 %%%%%%%%%%%%%%%%%%%% \
 %  %      %   %   %% \
 % o% o o %%...o  o % \
 %  o  % %%....% o  % \
 % %% o %%....%   o % \
 % o    %....%% o   % \
 % o%%  %...%       % \
 %   ooo%%o%%  %%% %% \
 % % %  %   %  %    % \
 % o %  o  %%       % \
 %    %    %@       % \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^70$/ {
  s/.*/\
SED Sokoban - LEVEL 70\
\
 %%%%%%%%%%%%%%%%%%%% \
 %  %  % %    %  %  % \
 %   o      o o     % \
 %% %  %o%%%o%%  %% % \
 %   o     o  %  o  % \
 % %%%o%%o%   % o   % \
 % %   o o  %%%%%% o% \
 % o  oo o  %@%.%...% \
 % %     %  % %.%...% \
 % %%%%%%%%%% %.....% \
 %            %.....% \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^71$/ {
  s/.*/\
SED Sokoban - LEVEL 71\
\
 %%%%%%%%%%%%%%%%%%%% \
 %  %     %  %%    %% \
 % o%   o %     %%  % \
 % o  o  %..%     o % \
 % o o  %....%   % %% \
 % o%  %......%%% o % \
 %   %  %....%  %o  % \
 % o  %%%%..%   %   % \
 %% o   %% % % o  o%% \
 %%% o    o%@o o%   % \
 %%%%   %       %   % \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^72$/ {
  s/.*/\
SED Sokoban - LEVEL 72\
\
 %%%%%%%%%%%%%%%%%%%% \
 %      ....%    %%%% \
 %      ....        % \
 % % %%%%%%%%%%     % \
 % %o   %      %%%..% \
 %  o   %oo%%%   %..% \
 % o %%% o   o   %..% \
 % o %   o o %  %%..% \
 %  %  oo % o %%   %% \
 %@%% o%  o  o     %% \
 %%       %%   %  %%% \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^73$/ {
  s/.*/\
SED Sokoban - LEVEL 73\
\
 %%%%%%%%%%%%%%%%%%%% \
 %        %   %@ %  % \
 % oo  %oo% % %  %% % \
 %  % o o %oo %     % \
 %% %  %  % % %  %  % \
 %   %%       %     % \
 %   % o %   %   %  % \
 % o %o %   %  o %..% \
 %%o %  %%%%    %...% \
 %  o          %....% \
 %   %  %     %.....% \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^74$/ {
  s/.*/\
SED Sokoban - LEVEL 74\
\
 %%%%%%%%%%%%%%%%%%%% \
 %     %   %%%%%    % \
 %% o  %   %%%%  o  % \
 %%%% oo   %..%  %  % \
 %  o  o  %%..%%%% %% \
 % o   %%%....   oo % \
 %  %o%   ....% % o % \
 % %  % o ..%%%o%   % \
 % %   o %..%   %%  % \
 %   o%  %%%%   % o%% \
 % %  %    @%      %% \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^75$/ {
  s/.*/\
SED Sokoban - LEVEL 75\
\
 %%%%%%%%%%%%%%%%%%%% \
 %   %   %    %   %@% \
 %   o  o     % o % % \
 %%o% o%%% %    oo% % \
 %  %  %.%%%  %o o  % \
 %  %o%....%  % %%% % \
 % o  %.....%%    % % \
 %%o  %.%....%oo o  % \
 %  %%%%%%..%% %  % % \
 %  o         o %%% % \
 %   %   %        % % \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^76$/ {
  s/.*/\
SED Sokoban - LEVEL 76\
\
 %%%%%%%%%%%%%%%%%%%% \
 % % % %   %@%%   % % \
 %             o    % \
 %  %%o% %%%%% o % %% \
 %%    %%.....%  %  % \
 %%o%%o%.....%%%o%o % \
 %   % %%.....%  % %% \
 %  o    %%..%%  %  % \
 % o %   o   o  ooo % \
 %% o  o% %  %  o   % \
 %   %%   %  %      % \
 %%%%%%%%%%%%%%%%%%%% \
/
  b endmap
}  
/^77$/ {
  s/.*/\
SED Sokoban - LEVEL 77\
\
 %%%%%%%%%%%%%%%%%%%% \
 %    %%   %    %   % \
 %  o  o     %% o   % \
 %% %%%%%  .%%%%%% %% \
  % %%  %%....%%%% %% \
 %% %%o %%%..%%     % \
 %      %... .% o o % \
 % o %% %% . %%% %%%% \
 % % o    %.%% % %    \
 % o o %   .%%%% %%   \
 % %  %% % %%  %  %%  \
 %%%%%%%  o%%o   o %  \
       %%      o %@%  \
        %  %% %%%%%%  \
        %%%%%%%       \
/
  b endmap
}  
/^78$/ {
  s/.*/\
SED Sokoban - LEVEL 78\
\
        %%%%%%%%%%%   \
        %         %   \
        %    o o  %   \
 %%%%%% % o %%%%% %   \
 %    %%%%% o  %%o%   \
 %       o o      %   \
 %          %% %% %   \
 %    %%@%%%%% %% %   \
 %    %%%%   % %% %%  \
 %....%      % o   %  \
 %....%      %     %  \
 %%%%%%      %%%%%%%  \
/
  b endmap
}  
/^79$/ {
  s/.*/\
SED Sokoban - LEVEL 79\
\
 %%%%%%%%%%%%%        \
 %           %        \
 % %%% oo    %        \
 %   % o  o  %        \
 %  o%%%%o%%%%%%      \
 % o %%        %%%%%  \
 %  oo o        ...%  \
 %%% %% oo%     ...%  \
   % %%   %     ...%  \
   %      %     ...%  \
   %%%@%%%%%%%%%%%%%  \
     %%%              \
/
  b endmap
}  
/^80$/ {
  s/.*/\
SED Sokoban - LEVEL 80\
\
   %%%%%%%%%%%%%%%%%  \
 %%%@%%         ...%  \
 %    %         ...%  \
 % o  %         ...%  \
 % oo %         ...%  \
 %% o %%%o%%%%%%%%%%  \
  % %%%  o %          \
 %%   o  o %          \
 %  o %  o %          \
 % o  %    %          \
 %  o %    %          \
 %    %    %          \
 %%%%%%%%%%%          \
/
  b endmap
}  
/^81$/ {
  s/.*/\
SED Sokoban - LEVEL 81\
\
               %%%%%  \
      %%%%%%%%%%   %  \
      %        %   %  \
      %  o o    oo %  \
      % %%%%% %% o %  \
      %oo   %o%% o %  \
      % %%% % %%o  %  \
 %%%%%% %%% o o    %  \
 %....        %%   %  \
 %....        %%%%%%  \
 %....        %       \
 %%%%%%%%%%%@%%       \
           %%%        \
/
  b endmap
}  
/^82$/ {
  s/.*/\
SED Sokoban - LEVEL 82\
\
     %%%%%%           \
  %%%%    %           \
  %    %% %           \
  % o     %           \
 %%% %%%% %%%%%%%%    \
 %  o   o %%  ...%    \
 %   oo oo    ...%    \
 %    o  o%%  ...%    \
 %%@%% %% %%  ...%    \
  %%%  o  %%%%%%%%    \
  %   oo  %           \
  %    %  %           \
  %%%%%%%%%           \
/
  b endmap
}  
/^83$/ {
  s/.*/\
SED Sokoban - LEVEL 83\
\
 %%%%%%% %%%%%%%%%    \
 %     % %   %%  %    \
 % %%% % %   o   %    \
 % % o %%%   o   %    \
 %   oo      %%o %    \
 %    %%%%   %%  %    \
 %@%%%%%%%%%%%% %%    \
 %%%..    %%%%%o %    \
   %..    %%%%   %    \
   %..       oo  %    \
   %..    %%%% o %    \
   %..    %  %   %    \
   %%%%%%%%  %%%%%    \
/
  b endmap
}  
/^84$/ {
  s/.*/\
SED Sokoban - LEVEL 84\
\
 %%%%%%%              \
 %     %%%%%%%%%%     \
 %     %    %  %%     \
 % o   %   o o  %     \
 %  o  %  o %%  %     \
 % oo  %%o o    %     \
 %% %  %% %%%%%%%     \
 %% %  %%    ...%     \
 %  %o       ...%     \
 %   oo      ...%     \
 %     %%@%  ...%     \
 %%%%%%%%%%%%%%%%     \
/
  b endmap
}  
/^85$/ {
  s/.*/\
SED Sokoban - LEVEL 85\
\
 %%%%%%%%%%%%         \
 %      %   %%        \
 % o  o   %  %%%%%%   \
 %%%%  %%%%%      %   \
  %..  %     %%%% %   \
  %.%%%%  %%%%    %   \
  %....    %  o %%%%  \
  % ...%   % ooo%  %% \
 %%%.%%%% %%  o@o   % \
 %     %%%%% o %    % \
 % %.% o      o%%%o % \
 % %.%%%%%%%%  %  o % \
 % %..        %%  o % \
 % % %%%%%%% o % %  % \
 %   %     %       %% \
 %%%%%     %%%%%%%%%% \
/
  b endmap
}  
/^86$/ {
  s/.*/\
SED Sokoban - LEVEL 86\
\
 %%%%%%%%%%%%%%%%     \
 %       %@ %   %     \
 % % % % % o  oo%     \
 % %...% %ooo   %     \
 %  ...% % o  oo%%    \
 % %%.%% % %%    %    \
 % %...     o    %    \
 % %% %%%  %%%%%%%    \
 %    % %%%%          \
 %%%%%%               \
/
  b endmap
}  
/^87$/ {
  s/.*/\
SED Sokoban - LEVEL 87\
\
     %%%%%            \
  %%%%   %% %%%%%     \
  %  o    %%%   %     \
  % o@o o    o  %     \
  % %o%%%%%%%% %%     \
  % %  o  %     %     \
  % % o o % %   %     \
 %% %  o% % %%%%%     \
 %  %%    %     %     \
 %    o % %%%   %     \
 %%%%% %%  %....%     \
 %    o     ....%     \
 %         %....%     \
 %%%%%%%%%%%%%%%%     \
/
  b endmap
}  
/^88$/ {
  s/.*/\
SED Sokoban - LEVEL 88\
\
 %%%%%%%%%%%%%        \
 %........%%%%        \
 %...%%%% %  %%%%%    \
 %...%  %%%    o %    \
 %...oo     o o  %    \
 %  .%  o o% o  %%    \
 %...% %o%   o  %     \
 %.% % o   o    %     \
 %.  %o%%%o%%%%o%     \
 %%  %   o o    %     \
  %  %  o@o  %  %     \
  %  % %%%% o  o%     \
  %  %    %%%   %     \
  %  % oo % %%%%%     \
  %  %    %           \
  %%%%%%%%%           \
/
  b endmap
}  
/^89$/ {
  s/.*/\
SED Sokoban - LEVEL 89\
\
  %%%%%%%%%%%%%%%%%%  \
  %   o       ...%.%% \
  %       %%%%..... % \
  % %%%%%%%  %..... % \
  % %    o o %%....%% \
  % %  o % % %%%...%  \
  % % o@o o  %%%%% %  \
 %% %  o  o oo   o %  \
 %  %o% o%   % o%% %  \
 % %%    %% %% o % %  \
 % % o% o o  %     %  \
 % %         %%%%%%%  \
 % %%%%%%%%o%%   %    \
 %        %  o   %    \
 %%%%%%%%    %%%%%    \
        %%%  %        \
          %%%%        \
/
  b endmap
}  
/^90$/ {
  s/.*/\
SED Sokoban - LEVEL 90\
\
 %%%%%%%%%%%%%%%%%%%% \
 %..%    %          % \
 %.o  o  %oo  o%% o%% \
 %.o%  %%%  %% %%   % \
 %  % o %  oo   o   % \
 % %%%  % %  %o  %%%% \
 %  %% % o   %@ %   % \
 % o    o  %%.%%  o % \
 %  % o% o% o     %%% \
 %  %  %  %   %%%   % \
 %  %%%%%%%% %      % \
 %           %  %.%.% \
 %%o%%%%%%%%o%   ...% \
 %    .O  %    %%.%.% \
 % .O...O   o  .....% \
 %%%%%%%%%%%%%%%%%%%% \
                      \
/
  b endmap
}  
/SED Soko/ !{
  s/.*/there is no '&' level!/p
  q
}  

: endmap
# back to line 1 col 1
s/^/[H/
# show available commands
s,\(\n\)$,\1\1[ h j k l :q :r :z :gN ],
x  
/:p / !s/.*//
b ini


: zero

# welcome message
1 b welcome

# first map loading
2 b loadmap

# supporting arrow keys also
// {
  s/\[A/k/g
  s/\[B/j/g
  s/\[C/l/g
  s/\[D/h/g
}  

# command aliases
s//:z/g

# lowercase commands
y/HJKLQGZR/hjklqgzr/

# wipe trash (anything that is not command)
s/[^hjklqgzr:0-9]//g

# commands!
/^:/ {
  # quit
  /^:q/ q

  # refresh screen
  /^:z/ {
    s/.*/[2J/p
    s/.*/:p [refresh]/
    b ini
  }
  # goto level N (optional g)
  /^:g\{0,1\}\([0-9]\{1,\}\)$/ {
    s//\1/
    h
    x
    s/.*/:p [goto level &]/
    x
    b loadmap
  }
  # restarting level
  /^:r/ {
    s/.*/:p [restart]/
    x
    s/.*LEVEL \([0-9]\{1,\}\).*/\1/
    b loadmap
  }
}  


# here the party begins
: ini

# print message (XXX bad idea)
/^:p / {
  s/.*//
  #s//last command: /; s/$/       /p;
  #s/last .*// 
}  

# empty command, jump to end
/./ !{
  x
  b x
}  


# -------------[ LEFT ]--------------------------

/^h/ {

  # del current move and save others
  s///
  x

  # reset 't' status
  t zeroleft
  : zeroleft

  # clear path
  s/ @/@ /
  t x
  # push load
  s/ o@/o@ /
  t x

  # enter overdot
  s/\.@/! /
  t x
  # continue overdot
  s/\.!/!./
  t x
  # out overdot
  s/ !/@./
  t x

  # enter load overdot
  s/\.o@/O@ /
  t x
  # enter overdot with load
  s/\.O@/O! /
  t x
  # continue overdot with load
  s/\.O!/O!./
  t x
  # out load overdot / enter overdot
  s/ O@/o! /
  t x
  # out load overdot / continue overdot
  s/ O!/o!./
  t x
  # out overdot with load
  s/ o!/o@./
  t x
  # out overdot with load / enter overdot
  s/\.o!/O@./
  t x

  # can't pass
  b x

}  


# -------------[ RIGHT ]-------------------------

/^l/ {

  # del current move and save others
  s///
  x

  # reset 't' status
  t zerorght
  : zerorght

  # clear path
  s/@ / @/
  t x
  # push load
  s/@o / @o/
  t x

  # enter overdot
  s/@\./ !/
  t x
  # continue overdot
  s/!\./.!/
  t x
  # out overdot
  s/! /.@/
  t x

  # enter load overdot
  s/@o\./ @O/
  t x
  # enter overdot with load
  s/@O\./ !O/
  t x
  # continue overdot with load
  s/!O\./.!O/
  t x
  # out load overdot / enter overdot
  s/@O / !o/
  t x
  # out load overdot / continue overdot
  s/!O /.!o/
  t x
  # out overdot with load
  s/!o /.@o/
  t x
  # out overdot with load / enter overdot
  s/!o\./.@O/
  t x

  # can't pass
  b x
}  


# -------------[ DOWN ]--------------------------

/^j/ {

  # del current move and save others
  s///
  x

  # reset 't' status
  t zerodown
  : zerodown

  # clear path
  s/@\(.\{22\}\) / \1@/
  t x
  # push load
  s/@\(.\{22\}\)o\(.\{22\}\) / \1@\2o/
  t x

  # enter overdot
  s/@\(.\{22\}\)\./ \1!/
  t x
  # continue overdot
  s/!\(.\{22\}\)\./.\1!/
  t x
  # out overdot
  s/!\(.\{22\}\) /.\1@/
  t x

  # enter load overdot
  s/@\(.\{22\}\)o\(.\{22\}\)\./ \1@\2O/
  t x
  # enter overdot with load
  s/@\(.\{22\}\)O\(.\{22\}\)\./ \1!\2O/
  t x
  # continue overdot with load
  s/!\(.\{22\}\)O\(.\{22\}\)\./.\1!\2O/
  t x
  # out load overdot / enter overdot
  s/@\(.\{22\}\)O\(.\{22\}\) / \1!\2o/
  t x
  # out load overdot / continue overdot
  s/!\(.\{22\}\)O\(.\{22\}\) /.\1!\2o/
  t x
  # out overdot with load
  s/!\(.\{22\}\)o\(.\{22\}\) /.\1@\2o/
  t x
  # out overdot with load / enter overdot
  s/!\(.\{22\}\)o\(.\{22\}\)\./.\1@\2O/
  t x

  # target not free
  b x
}  


# ---------------[ UP ]--------------------------

/^k/ {

  # del current move and save others
  s///
  x

  # reset 't' status
  t zeroup
  : zeroup

  # clear path
  s/ \(.\{22\}\)@/@\1 /
  t x
  # push load
  s/ \(.\{22\}\)o\(.\{22\}\)@/o\1@\2 /
  t x

  # enter overdot
  s/\.\(.\{22\}\)@/!\1 /
  t x
  # continue overdot
  s/\.\(.\{22\}\)!/!\1./
  t x
  # out overdot
  s/ \(.\{22\}\)!/@\1./
  t x

  # enter load overdot
  s/\.\(.\{22\}\)o\(.\{22\}\)@/O\1@\2 /
  t x
  # enter overdot with load
  s/\.\(.\{22\}\)O\(.\{22\}\)@/O\1!\2 /
  t x
  # continue overdot with load
  s/\.\(.\{22\}\)O\(.\{22\}\)!/O\1!\2./
  t x
  # out load overdot / enter overdot
  s/ \(.\{22\}\)O\(.\{22\}\)@/o\1!\2 /
  t x
  # out load overdot / continue overdot
  s/ \(.\{22\}\)O\(.\{22\}\)!/o\1!\2./
  t x
  # out overdot with load
  s/ \(.\{22\}\)o\(.\{22\}\)!/o\1@\2./
  t x
  # out overdot with load / enter overdot
  s/\.\(.\{22\}\)o\(.\{22\}\)!/O\1@\2./
  t x

  # target not free
  b x
}  

# wrong command, do nothing
s/^.//
x  


# ----------------[ THE END ]-----------------
: x

# adding color codes
s/%/[46;36m&[m/g
s/[!@]/[33;1m&[m/g
s/O/[37;1m&[m/g
s/\./[31;1m&[m/g


# uncomment this line if you DON'T want colorized output (why not?)
### s/\[[0-9;]*m//g

# update screen
p  

# removing color codes from maze
s/\[[0-9;]*m//g

# no more messy boxes ('o'), level finished!
/%%%.*o.*%%%/ !{
  s/.*/[37;01m(( [31mV[32mI[33mC[34mT/
  s/$/[31mO[32mR[33mY[34m![37m ))[m/
  s/$/                                                   /
  # uncomment here if you DON'T want color or sound on victory
  # s///g ; s/\[[0-9;]*m//g
  p
  i \
  You're a master of this level. Try the next!
  q
}  

# save current position on hold space
x  

# skipping loop
2 d

# nice loop for accumulated moves
/./ {
  p
  b ini
}  

# The End ;(

### colorized by sedsed, a sed script debugger/indenter/tokenizer/HTMLizer
