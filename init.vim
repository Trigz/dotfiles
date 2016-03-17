set background=light
set ruler
set more
set autoread
set number
set hidden
set noautowrite
set showmode
set showcmd
set autoindent smartindent
set expandtab
set scrolloff=5
set sidescrolloff=5
set backspace=indent,eol,start
set linebreak
set cmdheight=2
set noerrorbells
set fileformats=unix,dos
set ff=unix
set tabstop=4
set shiftwidth=4
set softtabstop=4

filetype on
filetype plugin on
filetype indent on
filetype plugin indent on

set incsearch
set ignorecase
set hlsearch
set showmatch
set nowritebackup
set nobackup

let mapleader = ','

call plug#begin('~/.config/nvim/plugged')
Plug 'scrooloose/nerdtree'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/syntastic'
Plug 'bling/vim-airline'
Plug 'Chiel92/vim-autoformat'
Plug 'idanarye/vim-dutyl'
Plug 'morhetz/gruvbox'
Plug 'justmao945/vim-clang'
Plug 'davidhalter/jedi-vim'
Plug 'tell-k/vim-autopep8'
Plug 'fatih/vim-go'
Plug 'millermedeiros/vim-esformatter'
Plug 'tpope/vim-fugitive'
Plug 'Shougo/deoplete.nvim'
Plug 'romainl/flattened'
Plug 'vim-airline/vim-airline-themes'
Plug 'zchee/deoplete-go', { 'do': 'make'}
Plug 'elixir-lang/vim-elixir'
Plug 'landaire/deoplete-d'
call plug#end()

syntax on
colorscheme flattened_light

"Select all
nnoremap <leader>a ggVG

"Format
nnoremap <leader>fp :Autopep8<cr>
nnoremap <leader>fd ggVGgq
nnoremap <leader>fc :pyf /usr/local/Cellar/llvm/3.6.2/share/clang/clang-format.py<cr>
nnoremap <leader>fj :Esformatter<cr>

"Windows
nnoremap <leader>n :bn<CR>
nnoremap <leader>p :bp<CR>
nnoremap <leader>cb :bp<bar>bd#<CR>
nnoremap <leader>cw <C-w>q
nnoremap <leader>ss <C-w><C-v>
nnoremap <leader>su <C-w><C-s>
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

"Git
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gc :Gcommit<CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gl :Glog<bar>copen<CR>

"NERDTree
nnoremap <leader>t :NERDTree<CR>

"Terminal
nnoremap <leader>cmd :terminal<CR>

" Syntastic
let g:syntastic_error_symbol = 'X'
let g:syntastic_warning_symbol = '!'
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_python_exec = '/usr/local/bin/python3'
let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = '-std=c++14 -stdlib=libc++'
let g:syntastic_javascript_checkers = ['eslint']

" vim-dutyl
let g:dutyl_stdImportPaths=['/usr/local/opt/dmd/include/d2']

" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_theme='solarized'

" vim-clang
let g:clang_format_auto = 1
let g:clang_check_syntax_auto = 0
let g:clang_diagsopt = ''

" jedi-vim
let g:jedi#force_py_version = 3

" vim-autopep8
let g:autopep8_disable_show_diff = 1

" ctrlp
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git'
let g:ctrlp_working_path_mode = 'r'

" deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#go#align_class = 1
set completeopt-=preview
