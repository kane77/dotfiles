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
Plugin 'Lokaltog/vim-easymotion'
Plugin 'kien/ctrlp.vim'
Plugin 'Solarized'
Plugin 'Raimondi/delimitMate'
Plugin 'pangloss/vim-javascript'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'jelera/vim-javascript-syntax'
Plugin 'honza/vim-snippets'
Plugin 'SirVer/ultisnips'
Plugin 'sjbach/lusty'
Plugin 'kien/ctrlp.vim'
" plugin from http://vim-scripts.org/vim/scripts.html
Plugin 'L9'
" Git plugin not hosted on GitHub
Plugin 'git://git.wincent.com/command-t.git'
Plugin 'https://github.com/tfnico/vim-gradle'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
let g:ycm_add_preview_to_completeopt=0
let g:ycm_confirm_extra_conf=0
set completeopt-=preview 
set nocompatible    " use vim defaults
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
set hidden
set ls=2            " allways show status line
set shm=at
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
unmap 
map ,n :NERDTreeToggle<CR>
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

syntax on           " syntax highlighing
if has("gui_running")
set guifont=Droid\ Sans\ Mono\ 9  " use this font
    set lines=999 columns=999
    set background=dark
    colorscheme solarized    " use this color scheme
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
map ,gb :! ./gradlew build
map <Leader>p :LustyJuggler<CR>
let g:CommandTMaxFiles=100000
set wildignore+=*.gif,*.jpg,*.png,.git,*.class
"----- write out html file

