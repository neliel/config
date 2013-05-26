if &termencoding == ""
  let &termencoding = &encoding
endif
set encoding=utf-8
set fileencodings=ucs-bom,utf-8,latin1

filetype plugin indent on
syntax on

" montre l'encodage dans la barre de status
set statusline=%<%f\ %h%m%r%=%{\"[\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"]\ \"}%k\ %-14.(%l,%c%V%)\ %P
set laststatus=2 " toujours montrer la barre de status

set nocompatible " use Vim defaults (much better!)
set bs=indent,eol,start " allow backspacing over everything in insert mode

set autoindent

set tabstop=4 "To insert space characters whenever the tab key is pressed
set shiftwidth=4 "To control the number of space characters that will be inserted when the tab key is pressed
set expandtab "To change the number of space characters inserted for indentation
set softtabstop=4 " makes the spaces feel like real tabs

set autoread " set to auto read when a file is changed from the outside
set hidden "change buffer without saving

set backup " keep a backup file
set backupdir=~/.vim/backup/ " choose the backup directory

set viminfo='20,\"50 " read/write a .viminfo file, don't store more than 50 lines of registers
set history=50 " keep 50 lines of command line history

set ruler " show the cursor position all the time
set number " montre le numéro de la ligne

set mouse=a " need gpm

set hlsearch " highlight search things
set incsearch " make search act like search in modern browsers

" maps pour le clavier bépo
"nnoremap r :
"nnoremap é v

"nnoremap n h
"vnoremap n h
"nnoremap m j
"vnoremap m j
"nnoremap z k
"vnoremap z k
"nnoremap ç l
"vnoremap ç l
"nnoremap w $
"vnoremap w $
"nnoremap j ^
"vnoremap j ^

"cnoremap z q
"cnoremap q z
"cnoremap j !
"cnoremap ! j

nnoremap a <C-r>
cnoremap mm bn<Enter>
cnoremap qq bd<Enter>

set spelllang=fr,en

colorscheme torte

nnoremap <F2> :set spell! spell?<CR>
nnoremap <F4> :%!indent<CR>
inoremap ii <Esc>

" supprime les espaces en fin de ligne
autocmd BufWrite * :%s/\s\+$//e
" In text files, always limit the width of text to 78 characters
autocmd BufRead *.txt set tw=78
" When editing a file, always jump to the last cursor position
autocmd BufReadPost *
\ if line("'\"") > 0 && line ("'\"") <= line("$") |
\   exe "normal! g'\"" |
\ endif
" don't write swapfile on most commonly used directories for NFS mounts or USB sticks
autocmd BufNewFile,BufReadPre /media/*,/mnt/* set directory=~/tmp,/var/tmp,/tmp

" Don't wake up system with blinking cursor:
" http://www.linuxpowertop.org/known.php
let &guicursor = &guicursor . ",a:blinkon0"

