let g:mapleader = ','

set encoding=utf-8
set fileencoding=utf-8

syntax enable
set hidden
"set nowrap
set pumheight=10
set ruler
set cmdheight=2
set iskeyword+=-
set mouse=a
set splitbelow
set splitright
set conceallevel=0

set tabstop=4
set shiftwidth=4
set smarttab
set expandtab
set smartindent

set laststatus=0
set number
set relativenumber
"set showtabline=2
set noshowmode
set nobackup
set nowritebackup
set updatetime=300
set timeoutlen=500
set clipboard=unnamedplus
"set audochdir

" automatically souce init.vim when saving this file
au! BufWritePost $MYVIMRC source %

" you can't stop me
"cmap w!! w !sudo tee %

set t_Co=256
set cursorline
set background=dark
colorscheme gotham256

" show matches and highlight searches
set ignorecase
set smartcase
set showmatch
set hlsearch

" save us from NeoVIM's default regex format
nnoremap / /\v
vnoremap / /\v

" settings for viewing whitespace characters
set showbreak=\\ " one space after the backslash
set listchars=tab:..,trail:_,nbsp:~,extends:>,precedes:<
