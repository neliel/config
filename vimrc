" .vimrc

if &termencoding == ""
  let &termencoding = &encoding
endif
set encoding=utf-8
set fileencodings=ucs-bom,utf-8,latin1

filetype plugin indent on
syntax on

set nocompatible
set modelines=0
set bs=indent,eol,start

set autoread                 " auto read again when file changed from outside
set hidden                   " allow editing without save and losing changes

set backup
set backupdir=~/.vim/backup/
set undofile
set undodir=~/.vim/undodir
set undolevels=1000          " max num of changes that can be undone
set undoreload=10000         " max num lines to save for undo on buffer reload


set viminfo='20,\"50
set history=50

set ttyfast

set autoindent
set nowrap
set textwidth=79

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

nnoremap / /\v
vnoremap / /\v
set incsearch
set hlsearch
set ignorecase
set smartcase

set ruler
set showcmd
set relativenumber
set cursorline
set colorcolumn=+1
set statusline=%<%f\ %h%m%r%=%{\"[\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"]\ \"}%k\ %-14.(%l,%c%V%)\ %P
set laststatus=2

set wildmenu
set wildmode=list:longest

" generic maps
inoremap ii <Esc>

" bépo keyboard maps
nnoremap r :
nnoremap x v

nnoremap x v
vnoremap x v
nnoremap v x
vnoremap v x
nnoremap l y
vnoremap l y
nnoremap j p
vnoremap j p

nnoremap p $
vnoremap p $
nnoremap b ^
vnoremap b ^

cnoremap z q
cnoremap j !

nnoremap a <C-r>
cnoremap çç bn<Enter>
cnoremap mm bd<Enter>

vnoremap <F5> "+x
vnoremap <F6> "+y
nnoremap <F7> "+gP

nnoremap <F2> :set spell! spell?<CR>
nnoremap <F3> :%!indent<CR>
nnoremap <F4> :r!date "+\%d.\%m.\%y \%H:\%M"<CR>

set spelllang=fr,en

colorscheme torte

autocmd BufWrite * :%s/\s\+$//e                     " delete trailing spaces
autocmd BufRead *.txt set tw=79                     " in .txt, limit the text width
autocmd BufReadPost *
\ if line("'\"") > 0 && line ("'\"") <= line("$") |
\   exe "normal! g'\"" |
\ endif                                             " jump to last cursor position when editing
autocmd BufNewFile,BufReadPre
\ /media/*,/mnt/* set directory=~/tmp,/var/tmp,/tmp " don't write swapfile on most commonly used directories for NFS mounts or USB sticks

execute pathogen#infect()

let &guicursor = &guicursor . ",a:blinkon0"         " Don't wake up system with blinking cursor


" plugins config
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1

nnoremap P :TlistToggle<CR>
let Tlist_Use_Horiz_Window = 1

au BufNewFile,BufRead *.pas,*.PAS set ft=delphi

