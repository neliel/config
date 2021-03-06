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

set autoread
set hidden

set backup
set backupdir=~/.vim/backup/
"set undofile
"set undodir=~/.vim/undodir
"set undolevels=1000
"set undoreload=10000

set viminfo='20,\"50
set history=50

set ttyfast
set noerrorbells

set autoindent
set nowrap
set textwidth=79

set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.

set tabstop=3
set shiftwidth=3
set softtabstop=3
set expandtab

nnoremap / /\v
vnoremap / /\v
set incsearch
set hlsearch
set ignorecase
set smartcase

set ruler
set showcmd
"set relativenumber
set statusline=%<%f\ %h%m%r%=%{\"[\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"]\ \"}%k\ %-14.(%l,%c%V%)\ %P
set laststatus=2

set wildmenu
set wildmode=list:longest
set wildignore=*.swp,*.bak

" generic maps
inoremap ii <Esc>

" b�po keyboard maps
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
cnoremap �� bn<Enter>
cnoremap mm bd<Enter>

vnoremap <F5> "+x
vnoremap <F6> "+y
nnoremap <F7> "+gP

"set spellang=fr,en
nnoremap <F2> :set spell! spell?<CR>

execute pathogen#infect()

if has("gui_running")
   set cursorline
"   set colorcolumn=+1
   set bg=light
   colorscheme solarized
else
   colorscheme peachpuff
endif

autocmd BufWrite * :%s/\s\+$//e
autocmd BufReadPost *
\ if line("'\"") > 0 && line ("'\"") <= line("$") |
\   exe "normal! g'\"" |
\ endif
autocmd BufNewFile,BufReadPre
\ /media/*,/mnt/* set directory=~/tmp,/var/tmp,/tmp

" plugins

let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1

nnoremap P :TlistToggle<CR>
let Tlist_Use_Horiz_Window = 1

au BufNewFile,BufRead *.pas,*.PAS set ft=delphi

let g:hasksyn_indent_search_backward = 100
let g:hasksyn_dedent_after_return = 1
let g:hasksyn_dedent_after_catchall_case = 1

