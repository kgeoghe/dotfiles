"filetype on
"filetype plugin indent on

syntax on
set title
set ls=2
set nu
set t_Co=256
colorscheme grb4

" set tab to 4 spaces for python files
autocmd Filetype python setlocal expandtab tabstop=4 shiftwidth=4 
" set tab to 2 spaces for bash and java files
autocmd Filetype sh setlocal expandtab tabstop=2 shiftwidth=2
autocmd Filetype java setlocal expandtab tabstop=2 shiftwidth=2 
