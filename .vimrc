" Vimrc file, http://phuzz.org

set nocompatible    " use vim defaults
set hidden
set ls=2            " allways show status line
set shm=at
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

syntax on           " syntax highlighing
if has("gui_running")
set guifont=Droid\ Sans\ Mono\ 9  " use this font
    set lines=999 columns=999
    colorscheme molokai    " use this color scheme
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
    au FileType cpp,c,java,sh,pl,php,asp  set autoindent
    au FileType cpp,c,java,sh,pl,php,asp  set smartindent
    au FileType cpp,c,java,sh,pl,php,asp  set cindent
    "au BufRead mutt*[0-9] set tw=72
    
    " Automatically chmod +x Shell and Perl scripts
    "au BufWritePost   *.sh             !chmod +x %
    "au BufWritePost   *.pl             !chmod +x %

    " File formats
    au BufNewFile,BufRead  *.pls    set syntax=dosini
    au BufNewFile,BufRead  modprobe.conf    set syntax=modconf
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
map ,f :FufFileWithCurrentBufferDir<CR>
map ,d :w<CR>:bdelete<CR>
map ,x "+x
map ,c "+y
map ,v "+gP
map ,a ggVG
map ,t :TlistToggle<CR>
map ,r :TlistHighlightTag
map <Leader>p :LustyJuggler<CR>
let g:CommandTMaxFiles=100000
set wildignore+=*.gif,*.jpg,*.png,.git,*.class
"----- write out html file
