" save a <shift> keypress
nnoremap ; :

" better nav for omnicomplete
"inoremap <expr> <c-j> ("\<C-n>")
"inoremap <expr> <c-k> ("\<C-p>")

" use alt + hjkl to resize windows
nnoremap <M-j> :resize -2<CR>
nnoremap <M-k> :resize +2<CR>
nnoremap <M-h> :vertical resize -2<CR>
nnoremap <M-l> :vertical resize +2<CR>

" easy caps
inoremap <c-u> <ESC>viwUi
nnoremap <c-u> viwU<Esc>

" TAB in general mode will move to next buffer
nnoremap <TAB>   :bnext<CR>
nnoremap <S-TAB> :bprevious<CR>

" better indent/outdent
vnoremap < <gv
vnoremap > >gv

" better window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" what do these do?
"nnoremap <Leader>o o<Esc>^Da
"nnoremap <Leader>O O<Esc>^Da

" toggle between absolute and relative line numbers
nnoremap <leader>n :call NumberToggle()<cr>

" delete trailing whitespace
nnoremap <leader>W :%s/\s\+$//<CR>:let @/=''<CR>

" comment a single line
nnoremap <leader>/ :Commentary<cr>
vnoremap <leader>/ :Commentary<cr>

" toggle viewing of whitespace characters
nmap <leader>l :set list!<CR>

" turn off highlighting of matches
nnoremap <leader><space> :nohlsearch<cr>
