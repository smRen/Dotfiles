"" Install vim plug
let autoload_plug_path = stdpath('data') . '/site/autoload/plug.vim'
if !filereadable(autoload_plug_path)
  silent execute '!curl -fLo ' . autoload_plug_path . '  --create-dirs
      \ "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
unlet autoload_plug_path

"" Plugins
call plug#begin()

""REPL
Plug 'jalvesaq/vimcmdline'

"" Surround
Plug 'tpope/vim-surround'

""Comment lines filetype specific
Plug 'tpope/vim-commentary'

"" Snippets
" Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

""Jinja syntax
Plug 'Glench/Vim-Jinja2-Syntax'

"" Vertical Line indentation
Plug 'Yggdroot/indentLine'

"" Highlight same objects/words
Plug 'RRethy/vim-illuminate'
"Plug 'itchyny/vim-cursorword'

"" Better syntax highlighting
" Plug 'sheerun/vim-polyglot'

"" Theming
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'edkolev/tmuxline.vim'
Plug 'ayu-theme/ayu-vim'
Plug 'joshdick/onedark.vim'
Plug 'morhetz/gruvbox'

"" Autocomplete and linting
Plug 'neoclide/coc.nvim', {'branch': 'release'}
"" Extensions for COC
"Plug 'neoclide/coc-tsserver', {'do': 'yarn install --frozen-lockfile'}
"Plug 'neoclide/coc-python', {'do': 'yarn install --frozen-lockfile'}
"Plug 'neoclide/coc-json', {'do': 'yarn install --frozen-lockfile'}
"Plug 'weirongxu/coc-explorer', {'do': 'yarn install --frozen-lockfile'}

"" Fuzzy finder
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

"" Neovim Grammar Modifications for python
Plug 'jeetsukumaran/vim-pythonsense' " Vim objects for python objects
" ac -> outer class, ic -> inner class, af -> outer function
" if -> inner function, ad -> outer docstring, id -> inner docstring

" Warping through text
Plug 'easymotion/vim-easymotion' " <leader> <leader> w

"" Vim-Slime -> Send text to other panel
Plug 'jpalardy/vim-slime'

call plug#end()

"" Remappings
" remap leader key to comma 
let mapleader = ","
let maplocalleader = "\\"

"" Theming
" colorscheme gruvbox
colorscheme onedark
set background=dark

"" General Config
filetype plugin indent on                   " File specific settings (like number of spaces in tabs) 
syntax enable                               " Nice looking colors
set number                                  " Absolute number
set ruler    
set termguicolors                           " enable true colors support
set lazyredraw
set smarttab                                " set tabs for a shifttabs logic    
set expandtab                               " expand tabs into spaces    
set autoindent                              " indent when moving to the next line while writing code
set cursorline                              " shows line under the cursor's line
hi clear CursorLine
hi CursorLine gui=underline cterm=underline
hi link illuminatedWord Visual
set showmatch                               " shows matching part of bracket pairs (), [], {}
"set scrolloff=3                            " let 10 lines before/after cursor during scroll
set clipboard+=unnamedplus                       " use system clipboard
set undofile                                " maintain undo history   
set undodir=~/.config/nvim/undodir	    " undo directory
set mouse=a				    " enable mouse
set splitbelow                              " split goes below
set splitright                              " split goes right
set wildmode=longest,full,list              " Menu for tab complete files

"" Airline Configs
let g:tmuxline_powerline_separators = 1
let g:airline_powerline_fonts = 1
let g:airline_theme='onedark'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline#extensions#tabline#ignore_bufadd_pat = 'defx|gundo|nerd_tree|startify|tagbar|undotree|vimfiler'

" Terminal stuff
" Launch terminal below
nnoremap <F1> :10sp term://zsh <CR>
" Escape terminal with escape
tnoremap <Esc> <C-\><C-n>		      	
" CTRL-R paste in insert mode inside terminal
tnoremap <expr> <M-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'
" Show job id
nnoremap <leader><C-i> :echo b:terminal_job_id <CR>

"" Vim-slime config
let g:slime_target = "neovim"
let g:slime_python_ipython = 1

"" Buffer/Window shortcut
nnoremap <C-W><leader>q :bd! <CR>
nnoremap <leader>n :bn <CR>
nnoremap <leader>p :bp! <CR>
" Movement between windows
tnoremap <M-h> <C-\><C-N><C-w>h
tnoremap <M-j> <C-\><C-N><C-w>j
tnoremap <M-k> <C-\><C-N><C-w>k
tnoremap <M-l> <C-\><C-N><C-w>l
inoremap <M-h> <C-\><C-N><C-w>h
inoremap <M-j> <C-\><C-N><C-w>j
inoremap <M-k> <C-\><C-N><C-w>k
inoremap <M-l> <C-\><C-N><C-w>l
nnoremap <M-h> <C-w>h
nnoremap <M-j> <C-w>j
nnoremap <M-k> <C-w>k
nnoremap <M-l> <C-w>l

"" Handle html tab spacing
autocmd BufRead,BufNewFile *.htm,*.html,*.js,*.css setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab

"" FZF
nnoremap <C-p> :Files <CR>
nnoremap <M-p> :Buffers <CR>

"" Save cursor and window position when moving between buffers
"autocmd! BufWinLeave * let b:winview = winsaveview()
"autocmd! BufWinEnter * if exists('b:winview') | call winrestview(b:winview) | unlet b:winview
" Save current view settings on a per-window, per-buffer basis.

function! AutoSaveWinView()
    if !exists("w:SavedBufView")
        let w:SavedBufView = {}
    endif
    let w:SavedBufView[bufnr("%")] = winsaveview()
endfunction

" Restore current view settings.
function! AutoRestoreWinView()
    let buf = bufnr("%")
    if exists("w:SavedBufView") && has_key(w:SavedBufView, buf)
        let v = winsaveview()
        let atStartOfFile = v.lnum == 1 && v.col == 0
        if atStartOfFile && !&diff
            call winrestview(w:SavedBufView[buf])
        endif
        unlet w:SavedBufView[buf]
    endif
endfunction

" When switching buffers, preserve window view.
if v:version >= 700
    autocmd BufLeave * call AutoSaveWinView()
    autocmd BufEnter * call AutoRestoreWinView()
endif

""""""""""""" Coc Settings """""""""""""""""
" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
if has('patch8.1.1068')
  " Use `complete_info` if your (Neo)Vim version supports it.
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current line.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Introduce function text object
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use <TAB> for selections ranges.
" NOTE: Requires 'textDocument/selectionRange' support from the language server.
" coc-tsserver, coc-python are the examples of servers that support it.
" nmap <silent> <TAB> <Plug>(coc-range-select)
" xmap <silent> <TAB> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings using CoCList:
" Show all diagnostics.
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

"" Snippets
" inoremap <silent><expr> <TAB>
"       \ pumvisible() ? coc#_select_confirm() :
"       \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
"       \ <SID>check_back_space() ? "\<TAB>" :
"       \ coc#refresh()
" 
" function! s:check_back_space() abort
"   let col = col('.') - 1
"   return !col || getline('.')[col - 1]  =~# '\s'
" endfunction

" let g:coc_snippet_next = '<tab>'

" Use <C-l> for trigger snippet expand.
" imap <C-l> <Plug>(coc-snippets-expand)
" 
" " Use <C-j> for select text for visual placeholder of snippet.
" vmap <C-j> <Plug>(coc-snippets-select)
" 
" " Use <C-j> for jump to next placeholder, it's default of coc.nvim
" let g:coc_snippet_next = '<c-j>'
" 
" " Use <C-k> for jump to previous placeholder, it's default of coc.nvim
" let g:coc_snippet_prev = '<c-k>'
" 
" " Use <C-j> for both expand and jump (make expand higher priority.)
" imap <C-j> <Plug>(coc-snippets-expand-jump)
