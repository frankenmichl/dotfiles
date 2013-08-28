execute pathogen#infect()

" settings for c/c++ coding
syntax on
set number
set tabstop=2
set shiftwidth=2

" Linebreak on 500 characters
set lbr
set tw=500

set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines


" Delete trailing white space on save, useful for Python and CoffeeScript ;)
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()
autocmd BufWrite *.coffee :call DeleteTrailingWS()

set history=700

" enable filetype plugins
filetype plugin on
filetype indent on

" set to auto read a file when changed on disk from outside
set autoread

" When searching try to be smart about cases 
set smartcase
" Highlight search results
set hlsearch
" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" For regular expressions turn magic on
set magic

" Show matching brackets when text indicator is over them
set showmatch
" How many tenths of a second to blink when matching brackets
set mat=2

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500



" colorscheme settings 
colorscheme desert
set background=dark
" Set extra options when running in GUI mode
if has("gui_running")
    set guioptions-=T
    set guioptions+=e
    set t_Co=256
    set guitablabel=%M\ %t
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile

" statusbar
set laststatus=2
"set statusline=%{GitBranch()}
"set statusline+=%#todo#  "switch to todo highlight
"set statusline+=%F       "full filename
"set statusline+=%#error# "switch to error highlight
"set statusline+=%y       "filetype
"set statusline+=%*       "switch back to normal statusline highlight
"set statusline+=%l       "line number
"set statusline+=%{FileSize()}
set statusline=[%{GitBranch()}]\ %t\ [%{strlen(&fenc)?&fenc:'none'},%{&ff}]\ %h%m%r%y%=%c,%l/%L\ %P\ [%{FileSize()}]

function! FileSize()
	let bytes = getfsize(expand("%:p"))
	if bytes <= 0
		return ""
	endif
	if bytes < 1024
		return bytes
	else
		return (bytes / 1024) . "K"
	endif
endfunction


" nerdtree keymapping
map <C-n> :NERDTreeToggle<CR>
" cancel search / clear highlight
"nnoremap <esc> :noh<return><esc>
nnoremap <F3> :noh<CR>

" not yet working - ctags
:set tags=./tags;~/work
let Tlist_Ctags_Cmd = "/usr/local/bin/ctags"
let Tlist_WinWidth = 50
map <F8> :!/usr/local/bin/ctags -R .<CR>
map <F4> :TlistToggle<CR>
nnoremap ü <C-]>
nnoremap Ü <C-O>
