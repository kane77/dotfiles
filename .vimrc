set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-surround'
Plugin 'bling/vim-airline'
Plugin 'molokai'
Plugin 'ervandew/supertab'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'morhetz/gruvbox'
Plugin 'kien/ctrlp.vim'
Plugin 'rainbow_parentheses.vim'
Plugin 'Solarized'
Plugin 'mattn/emmet-vim'
Plugin 'Raimondi/delimitMate'
Plugin 'jelera/vim-javascript-syntax'
" Plugin 'pangloss/vim-javascript'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'tmhedberg/matchit'
Plugin 'honza/vim-snippets'
Plugin 'cakebaker/scss-syntax.vim'
Plugin 'SirVer/ultisnips'
Plugin 'sjbach/lusty'
"Plugin 'marijnh/tern_for_vim'
Plugin 'tpope/vim-ragtag'
Plugin 'mileszs/ack.vim'
Plugin 'xolox/vim-notes'
Plugin 'xolox/vim-misc'
" plugin from http://vim-scripts.org/vim/scripts.html
Plugin 'L9'
" Git plugin not hosted on GitHub
" Plugin 'git://git.wincent.com/command-t.git'
Plugin 'https://github.com/tfnico/vim-gradle'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
set completeopt-=preview 
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['black',       'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]
" let g:javascript_conceal_function   = "ƒ"
" let g:javascript_conceal_null       = "█"
" let g:javascript_conceal_this       = "@"
" let g:javascript_conceal_return     = "⇚"
" let g:javascript_conceal_undefined  = "¿"
" let g:javascript_conceal_NaN        = "ℕ"
" let g:javascript_conceal_prototype  = "¶"
" let g:javascript_conceal_static     = "•"
" let g:javascript_conceal_super      = "Ω"

set hidden
set ls=2            " allways show status line
set shm=at
" set conceallevel=2
imap <C-c> <CR><Esc>O
set tabstop=4       " numbers of spaces of tab character
set expandtab
set shiftwidth=4    " numbers of spaces to (auto)indent
set scrolloff=5     " keep 3 lines when scrolling
set showcmd         " display incomplete commands
set hlsearch        " highlight searches
set incsearch       " do incremental searching
set ruler           " show the cursor position all the time
set visualbell t_vb=    " turn off error beep/flash
set novisualbell    " turn off visual bell
set nobackup        " do not keep a backup file
set number          " show line numbers
set ignorecase      " ignore case when searching


"Nerdtree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif


"set noignorecase   " don't ignore case
set title           " show title in console title bar
set ttyfast         " smoother changes
"set ttyscroll=0        " turn off scrolling, didn't work well with PuTTY
set modeline        " last lines in document sets vim mode
set modelines=3     " number lines checked for modelines
set shortmess=atI   " Abbreviate messages
set nostartofline   " don't jump to first character when paging
set whichwrap=b,s,h,l,<,>,[,]   " move freely between files
"set viminfo='20,<50,s10,h
set cursorline
set autoindent     " always set autoindenting on
set smartindent        " smart indent
filetype indent on
filetype plugin on
"set cindent            " cindent
set backupdir=~/.vim/backup
set directory=~/.vim/backup
set lazyredraw
set nowrap
let g:EasyMotion_leader_key='.'
"set noautoindent
"set nosmartindent
"set nocindent  

"set autowrite      " auto saves changes when quitting and swiching buffer
"set expandtab      " tabs are converted to spaces, use only when required
"set sm             " show matching braces, somewhat annoying...
"set nowrap         " don't wrap lines

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  "let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  "let g:ctrlp_use_caching = 0
endif


syntax on           " syntax highlighing
if has("gui_running")
    set guifont=Droid\ Sans\ Mono\ 10  " use this font
    set lines=999 columns=999
    set background=dark
    colorscheme gruvbox    " use this color scheme
else
    set background=dark        " adapt colors for background
endif

if has("autocmd")
    " Restore cursor position

    " Filetypes (au = autocmd)
    au FileType helpfile set nonumber      " no line numbers when viewing help
    au FileType helpfile nnoremap <buffer><cr> <c-]>   " Enter selects subject
    au FileType helpfile nnoremap <buffer><bs> <c-T>   " Backspace to go back
    
    " When using mutt, text width=72
    au FileType mail,tex set textwidth=72
    au FileType cpp,c,java,sh,pl,php,asp,jsp  set autoindent
    au FileType cpp,c,java,sh,pl,php,asp,jsp  set smartindent
    au FileType cpp,c,java,sh,pl,php,asp,jsp  set cindent
    "au BufRead mutt*[0-9] set tw=72
    
    " Automatically chmod +x Shell and Perl scripts
    "au BufWritePost   *.sh             !chmod +x %
    "au BufWritePost   *.pl             !chmod +x %

    " File formats
    au BufNewFile,BufRead  *.pls    set syntax=dosini
    au BufNewFile,BufRead  modprobe.conf    set syntax=modconf
    autocmd BufNewFile,Bufread *.jspf setfiletype jsp
    au VimEnter * RainbowParenthesesToggle
    au Syntax * RainbowParenthesesLoadRound
    au Syntax * RainbowParenthesesLoadSquare
    au Syntax * RainbowParenthesesLoadBraces
endif

" Keyboard mappings
map <silent> <C-N> :silent noh<CR> " turn off highlighted search
" map ,v :sp ~/.vimrc<cr> " edit my .vimrc file in a split
map ,e :e ~/.vimrc<cr>      " edit my .vimrc file
map ,u :source ~/.vimrc<cr> " update the system settings from my vimrc file
set wmh=0
map <A-2> <C-W>j<C-W>_<C-W>\| 
map <A-4> <C-W>h<C-W>_<C-W>\| 
map <A-6> <C-W>l<C-W>_<C-W>\| 
map <A-8> <C-W>k<C-W>_<C-W>\| 
map <A-7> :bNext<CR>
map ,f :NERDTreeFind<CR>
map ,d :w<CR>:bdelete<CR>
map ,x "+x
map ,c "+y
map ,v "+gP
map ,a ggVG
map ,m :cd /home/harvan/ws/nwp-parent/nwp-webapp/src/main/webapp<CR>
map ,tt :TernDefPreview<CR>
map ,td :TernDoc<CR>
map ,r :TlistHighlightTag
map ,gb :! ./gradlew build
map ,p :CtrlP /home/harvan/ws/nwp-parent/nwp-webapp/src/main/webapp<CR>
map ,o :CtrlP /home/harvan/ws/nwp-parent/<CR>
map ,gf T"lvt"gf
map ,n :NERDTreeToggle<CR>
map <Leader>p :LustyJuggler<CR>
map <Leader>b :CtrlPBuffer<CR>
map ,h y:silent grep! "<C-R>"" <Bar> cw<CR><CR>
map <F4> :execute "vimgrep /" . expand("<cword>") . "/j **" <Bar> cw<CR><CR>
map ,b :grep! "\#TODO\#" <Bar> cw<CR><CR>
"nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR><CR>
let g:CommandTMaxFiles=100000
set wildignore+=*.gif,*.jpg,*.png,.git,*.class,*.ogg,*.doc,*.xls,*.xlsx
set wildignore+=*.zip,*.o,*.pdf,*.mpg,*.ZIP,*.MPG,*.PDF,*.jar,*.class
set wildignore+=*.docx,*.ppt,*.rar
let g:ctrlp_custom_ignore=&wildignore
let g:CommandTWildIgnore=&wildignore 
let g:tern_request_timeout=200
let g:ctrlp_working_path_mode = 0
"----- write out html file

