set nocompatible
let mapleader=","
execute pathogen#infect()
syntax enable
"=====================================================================
filetype off                  " required
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
"Plugin 'gregsexton/MatchTag'
Plugin 'VundleVim/Vundle.vim'
Plugin 'chriskempson/base16-vim'
Plugin 'bling/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'bling/vim-bufferline'
Plugin 'jiangmiao/auto-pairs'
Plugin 'othree/html5.vim'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'godlygeek/tabular'
Plugin 'myusuf3/numbers.vim'
Plugin 'mileszs/ack.vim'
Plugin 'tpope/vim-commentary'
Plugin 'KabbAmine/vCoolor.vim'
Plugin 'ctrlp.vim'
Plugin 'indentLine'
Plugin 'matchit'
Plugin 'nerdtree'
Plugin 'sparkup'
Plugin 'syntastic'
Plugin 'vim-colors-solarized'
Plugin 'vim-multiple-cursors'
Plugin 'vim-surround'
Plugin 'gcmt/wildfire.vim'
Plugin 'YouCompleteMe'
Plugin 'NLKNguyen/papercolor-theme'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
call vundle#end()
filetype plugin indent on
"=====================================================================
set encoding=utf-8
set ruler                             " show the cursor position all the time
set showmatch                         " highlight matching braces
set showmode                          " show insert/replace/visual mode
set fileencoding=utf-8                " encoding used when saving file
set nobackup                          " do not keep the backup~ file
set noswapfile                        " disable swapping
set nohlsearch                        " do not highlight search results
set ignorecase                        " do case insensitive search...
set smartcase                         " ...unless capital letters are used
set incsearch                         " search as characters are entered
set gdefault                          " like using g flag in replacement :%s/search/replace/g
set tabstop=3                         " number of visual spaces per TAB
set softtabstop=3                     " number of spaces in tab when editing
set expandtab                         " tabs are spaces
set shiftwidth=3
set number                            " Show line numbers
set nowrap                            " Dont break long lines
set linebreak                         " Break lines in spaces, not middle of words
set showbreak=>\
set showcmd                           " Show command in bottom bar
set cursorline                        " highlight current line
set wildmenu                          " visual autocomplete for command menu
set laststatus=2                      " Always show the statusbar
set smartindent
set autoindent
set relativenumber
set numberwidth=1                     " set numberwidth to smallest width possible
set scrolloff=3                       " I always have 3 line above or below the cursor
set guicursor+=n:hor20-Cursor/lCursor " change cursor shape to _
set guicursor+=a:blinkon0             " Dont blink
set lcs=tab:▸\ ,trail:·,eol:¬,nbsp:_
set list
set ttyfast
set foldmethod=marker
set timeout                           " Fixes slow O inserts (all three)
set timeoutlen=1000                   " Fixes slow O inserts (all three)
set ttimeoutlen=100                   " Fixes slow O inserts (all three)
set guioptions-=m                     " remove menu bar
set guioptions-=T                     " remove toolbar
set guioptions-=r                     " remove right-hand scroll bar
set guioptions-=L                     " remove left-hand scroll bar
"=====================================================================
if has('gui_running')
    set guifont=Fira\ Mono\ for\ Powerline\ 11
endif
"=====================================================================
"Plugin Configurations
let g:airline_powerline_fonts=1
let g:indentLine_char = '│'
let g:indentLine_color_term = 239
let g:ctrlp_working_path_mode = 'c'
let g:ycm_seed_identifiers_with_syntax = 1
let g:airline_theme='wombat'
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsExpandTrigger="<s-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"
"=====================================================================
set t_Co=256
set background=dark
colorscheme PaperColor
" set t_Co=16
" let g:solarized_termcolors=16
" colorscheme solarized
" set background=dark
"=====================================================================
nnoremap \ ,
nnoremap Y y$
nnoremap <F5> :w<cr>
nnoremap <leader>p     "0p
nnoremap <leader>P     "0P
nnoremap <leader>cp    "+p
nnoremap <leader>cP    "+P
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l
nnoremap <leader>nt :NERDTree <cr>
nnoremap <leader>st ^v$h
nnoremap <leader>ev :e! $MYVIMRC<cr>
inoremap zz <esc>zzi
inoremap <F5> <esc>:w<cr>
nnoremap / /\v
vnoremap / /\v
nnoremap ? ?\v
vnoremap ? ?\v
"abbreviations
inoreabbrev teh the
cnoreabbrev Wq wq
cnoreabbrev tn tabnew
"autocmdmands
augroup vimscrips
   autocmd!
   autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END
"=====================================================================
                                " vim function
"name must start uppercase, inside a function you can do whatever you do in
"command line, exclmation point is to replace it when using :so % to force
"vim to read vimrc without exiting vim. without it we will get an error. 

" function! AddHelloToTop ()
"       normal HOhello there.0
"       s/hello there/hi/
"       return "we add a message
" endfunction

"forcing tab in insert mode to act as if it is c-n, so it brings up
"autocomplition
" function! InsertTabWrapper()
"    let col = col(".") - 1
"    if !col || getline(".")[col - 1] !~ '\k'
"       return "\<tab>"
"    else
"       return "\<c-n>"
" endfunction
" inoremap <tab> <c-r>=InsertTabWrapper()<cr>
" inoremap <s-tab> <c-p>
                                   "command
"making a command, which its name should start uppercase
"exclmation point is to replace it when using :so % to force
"vim to read vimrc without exiting vim. without it we will get an error. 

" comman! Hello call AddHelloToTop()
