let SessionLoad = 1
if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
inoremap <silent> <Plug>(neocomplcache_start_omni_complete) 
inoremap <silent> <Plug>(neocomplcache_start_auto_complete_no_select) 
inoremap <silent> <Plug>(neocomplcache_start_auto_complete) =neocomplcache#mappings#popup_post()
inoremap <silent> <expr> <Plug>(neocomplcache_start_unite_quick_match) unite#sources#neocomplcache#start_quick_match()
inoremap <silent> <expr> <Plug>(neocomplcache_start_unite_complete) unite#sources#neocomplcache#start_complete()
imap <silent> <Plug>IMAP_JumpBack =IMAP_Jumpfunc('b', 0)
imap <silent> <Plug>IMAP_JumpForward =IMAP_Jumpfunc('', 0)
inoremap <silent> <expr> <Plug>(neosnippet_start_unite_snippet) unite#sources#neosnippet#start_complete()
inoremap <silent> <expr> <Plug>(neosnippet_jump) neosnippet#mappings#jump_impl()
inoremap <silent> <expr> <Plug>(neosnippet_expand) neosnippet#mappings#expand_impl()
inoremap <silent> <expr> <Plug>(neosnippet_jump_or_expand) neosnippet#mappings#jump_or_expand_impl()
inoremap <silent> <expr> <Plug>(neosnippet_expand_or_jump) neosnippet#mappings#expand_or_jump_impl()
inoremap <expr> <BS> neocomplcache#smart_close_popup()."\"
nmap <NL> <Plug>IMAP_JumpForward
vmap <NL> <Plug>IMAP_JumpForward
xmap  <Plug>(neosnippet_expand_target)
smap  <Plug>(neosnippet_expand_or_jump)
nnoremap <silent>  :CtrlPBufTag
vmap  <Plug>TwitvimVisual
nmap d :cs find d =expand("<cword>")	
nmap i :cs find i ^=expand("<cfile>")$
nmap f :cs find f =expand("<cfile>")	
nmap e :cs find e =expand("<cword>")	
nmap t :cs find t =expand("<cword>")	
nmap c :cs find c =expand("<cword>")	
nmap g :cs find g =expand("<cword>")	
nmap s :cs find s =expand("<cword>")	
vnoremap <silent> 9 :TCommentMaybeInline count=9
nnoremap <silent> 9 :TComment count=9
onoremap <silent> 9 :TComment count=9
vnoremap <silent> 8 :TCommentMaybeInline count=8
nnoremap <silent> 8 :TComment count=8
onoremap <silent> 8 :TComment count=8
vnoremap <silent> 7 :TCommentMaybeInline count=7
nnoremap <silent> 7 :TComment count=7
onoremap <silent> 7 :TComment count=7
vnoremap <silent> 6 :TCommentMaybeInline count=6
nnoremap <silent> 6 :TComment count=6
onoremap <silent> 6 :TComment count=6
vnoremap <silent> 5 :TCommentMaybeInline count=5
nnoremap <silent> 5 :TComment count=5
onoremap <silent> 5 :TComment count=5
vnoremap <silent> 4 :TCommentMaybeInline count=4
nnoremap <silent> 4 :TComment count=4
onoremap <silent> 4 :TComment count=4
vnoremap <silent> 3 :TCommentMaybeInline count=3
nnoremap <silent> 3 :TComment count=3
onoremap <silent> 3 :TComment count=3
vnoremap <silent> 2 :TCommentMaybeInline count=2
nnoremap <silent> 2 :TComment count=2
onoremap <silent> 2 :TComment count=2
vnoremap <silent> 1 :TCommentMaybeInline count=1
nnoremap <silent> 1 :TComment count=1
onoremap <silent> 1 :TComment count=1
noremap ca :call tcomment#SetOption("as", input("Comment as: ", &filetype, "customlist,tcomment#Complete"))
noremap <silent> cc :call tcomment#SetOption("count", v:count1)
noremap s :TCommentAs =&ft_
noremap n :TCommentAs =&ft 
noremap a :TCommentAs 
noremap b :TCommentBlock
noremap <silent> i v:TCommentInline mode=I#
noremap <silent> r :TCommentRight
noremap   :TComment 
noremap <silent> p m`vip:TComment``
vnoremap <silent>  :TCommentMaybeInline
nnoremap <silent>  :TComment
onoremap <silent>  :TComment
noremap ' `
map Q gq
xmap S <Plug>VSurround
vmap [% [%m'gv``
nmap \d :cs find d =expand("<cword>")	
nmap \i :cs find i ^=expand("<cfile>")$
nmap \f :cs find f =expand("<cfile>")	
nmap \e :cs find e =expand("<cword>")	
nmap \t :cs find t =expand("<cword>")	
nmap \c :cs find c =expand("<cword>")	
nmap \g :cs find g =expand("<cword>")	
nmap \s :cs find s =expand("<cword>")	
vmap ]% ]%m'gv``
noremap ` '
vmap a% [%v]%
nmap cs <Plug>Csurround
nmap ds <Plug>Dsurround
vmap gx <Plug>NetrwBrowseXVis
nmap gx <Plug>NetrwBrowseX
xnoremap <silent> gC :TCommentMaybeInline!
nnoremap <silent> gCb :let w:tcommentPos = getpos(".") | call tcomment#SetOption("mode_extra", "B") | set opfunc=tcomment#OperatorLineg@
nnoremap <silent> gCc :let w:tcommentPos = getpos(".") | set opfunc=tcomment#OperatorLineAnywayg@$
nnoremap <silent> gC :let w:tcommentPos = getpos(".") | set opfunc=tcomment#OperatorAnywayg@
xnoremap <silent> gc :TCommentMaybeInline
nnoremap <silent> gcb :let w:tcommentPos = getpos(".") | call tcomment#SetOption("mode_extra", "B") | set opfunc=tcomment#OperatorLineg@
nnoremap <silent> gcc :let w:tcommentPos = getpos(".") | set opfunc=tcomment#OperatorLineg@$
nnoremap <silent> gc9c :let w:tcommentPos = getpos(".") | call tcomment#SetOption("count", 9) | set opfunc=tcomment#Operatorg@
nnoremap <silent> gc8c :let w:tcommentPos = getpos(".") | call tcomment#SetOption("count", 8) | set opfunc=tcomment#Operatorg@
nnoremap <silent> gc7c :let w:tcommentPos = getpos(".") | call tcomment#SetOption("count", 7) | set opfunc=tcomment#Operatorg@
nnoremap <silent> gc6c :let w:tcommentPos = getpos(".") | call tcomment#SetOption("count", 6) | set opfunc=tcomment#Operatorg@
nnoremap <silent> gc5c :let w:tcommentPos = getpos(".") | call tcomment#SetOption("count", 5) | set opfunc=tcomment#Operatorg@
nnoremap <silent> gc4c :let w:tcommentPos = getpos(".") | call tcomment#SetOption("count", 4) | set opfunc=tcomment#Operatorg@
nnoremap <silent> gc3c :let w:tcommentPos = getpos(".") | call tcomment#SetOption("count", 3) | set opfunc=tcomment#Operatorg@
nnoremap <silent> gc2c :let w:tcommentPos = getpos(".") | call tcomment#SetOption("count", 2) | set opfunc=tcomment#Operatorg@
nnoremap <silent> gc1c :let w:tcommentPos = getpos(".") | call tcomment#SetOption("count", 1) | set opfunc=tcomment#Operatorg@
nnoremap <silent> gc :if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") | set opfunc=tcomment#Operatorg@
xmap gS <Plug>VgSurround
omap ic :normal vic
vnoremap ic :silent call tcomment#TextObjectInlineComment()
nmap ySS <Plug>YSsurround
nmap ySs <Plug>YSsurround
nmap yss <Plug>Yssurround
nmap yS <Plug>YSurround
nmap ys <Plug>Ysurround
vnoremap <silent> <Plug>NetrwBrowseXVis :call netrw#BrowseXVis()
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#BrowseX(expand((exists("g:netrw_gx")? g:netrw_gx : '<cfile>')),netrw#CheckIfRemote())
vmap <silent> <Plug>IMAP_JumpBack `<i=IMAP_Jumpfunc('b', 0)
vmap <silent> <Plug>IMAP_JumpForward i=IMAP_Jumpfunc('', 0)
vmap <silent> <Plug>IMAP_DeleteAndJumpBack "_<Del>i=IMAP_Jumpfunc('b', 0)
vmap <silent> <Plug>IMAP_DeleteAndJumpForward "_<Del>i=IMAP_Jumpfunc('', 0)
nmap <silent> <Plug>IMAP_JumpBack i=IMAP_Jumpfunc('b', 0)
nmap <silent> <Plug>IMAP_JumpForward i=IMAP_Jumpfunc('', 0)
nmap <Nul><Nul>d :vert scs find d =expand("<cword>")
nmap <Nul><Nul>i :vert scs find i ^=expand("<cfile>")$	
nmap <Nul><Nul>f :vert scs find f =expand("<cfile>")	
nmap <Nul><Nul>e :vert scs find e =expand("<cword>")
nmap <Nul><Nul>t :vert scs find t =expand("<cword>")
nmap <Nul><Nul>c :vert scs find c =expand("<cword>")
nmap <Nul><Nul>g :vert scs find g =expand("<cword>")
nmap <Nul><Nul>s :vert scs find s =expand("<cword>")
nmap <Nul>d :scs find d =expand("<cword>")	
nmap <Nul>i :scs find i ^=expand("<cfile>")$	
nmap <Nul>f :scs find f =expand("<cfile>")	
nmap <Nul>e :scs find e =expand("<cword>")	
nmap <Nul>t :scs find t =expand("<cword>")	
nmap <Nul>c :scs find c =expand("<cword>")	
nmap <Nul>g :scs find g =expand("<cword>")	
nmap <Nul>s :scs find s =expand("<cword>")	
nmap <silent> <Plug>RestoreWinPosn :call RestoreWinPosn()
nmap <silent> <Plug>SaveWinPosn :call SaveWinPosn()
nmap <SNR>70_WE <Plug>AlignMapsWrapperEnd
map <SNR>70_WS <Plug>AlignMapsWrapperStart
nnoremap <silent> <Plug>SurroundRepeat .
xnoremap <silent> <Plug>(neosnippet_register_oneshot_snippet) :call neosnippet#mappings#_register_oneshot_snippet()
xnoremap <silent> <Plug>(neosnippet_expand_target) :call neosnippet#mappings#_expand_target()
xnoremap <silent> <Plug>(neosnippet_get_selected_text) :call neosnippet#helpers#get_selected_text(visualmode(), 1)
snoremap <silent> <expr> <Plug>(neosnippet_jump) neosnippet#mappings#jump_impl()
snoremap <silent> <expr> <Plug>(neosnippet_expand) neosnippet#mappings#expand_impl()
snoremap <silent> <expr> <Plug>(neosnippet_jump_or_expand) neosnippet#mappings#jump_or_expand_impl()
snoremap <silent> <expr> <Plug>(neosnippet_expand_or_jump) neosnippet#mappings#expand_or_jump_impl()
noremap <F2> :GundoToggle
noremap <F12> :call system("urxvtc -cd " . expand("%:p:h"))
noremap <F5> :make
inoremap <expr>  neocomplcache#cancel_popup()
imap S <Plug>ISurround
imap s <Plug>Isurround
inoremap <expr>  neocomplcache#undo_completion()
inoremap <expr>  neocomplcache#smart_close_popup()."\"
inoremap <expr> 	 pumvisible() ? "\" : "\	"
imap <NL> <Plug>IMAP_JumpForward
imap  <Plug>(neosnippet_expand_or_jump)
inoremap <expr>  neocomplcache#complete_common_string()
imap  <Plug>Isurround
inoremap  u
inoremap <expr>  neocomplcache#close_popup()
inoremap <silent> 9 :TComment count=9
inoremap <silent> 8 :TComment count=8
inoremap <silent> 7 :TComment count=7
inoremap <silent> 6 :TComment count=6
inoremap <silent> 5 :TComment count=5
inoremap <silent> 4 :TComment count=4
inoremap <silent> 3 :TComment count=3
inoremap <silent> 2 :TComment count=2
inoremap <silent> 1 :TComment count=1
inoremap s :TCommentAs =&ft_
inoremap n :TCommentAs =&ft 
inoremap a :TCommentAs 
inoremap b :TCommentBlock
inoremap <silent> i v:TCommentInline mode=#
inoremap <silent> r :TCommentRight
inoremap   :TComment 
inoremap <silent> p :norm! m`vip:TComment``
inoremap <silent>  :TComment
vmap ô <Plug>TwitvimVisual
map örwp <Plug>RestoreWinPosn
map öswp <Plug>SaveWinPosn
map ött <Plug>AM_tt
map ötsq <Plug>AM_tsq
map ötsp <Plug>AM_tsp
map ötml <Plug>AM_tml
map ötab <Plug>AM_tab
map öm= <Plug>AM_m=
map öt@ <Plug>AM_t@
map öt~ <Plug>AM_t~
map öt? <Plug>AM_t?
map öw= <Plug>AM_w=
map öts= <Plug>AM_ts=
map öts< <Plug>AM_ts<
map öts; <Plug>AM_ts;
map öts: <Plug>AM_ts:
map öts, <Plug>AM_ts,
map öt= <Plug>AM_t=
map öt< <Plug>AM_t<
map öt; <Plug>AM_t;
map öt: <Plug>AM_t:
map öt, <Plug>AM_t,
map öt# <Plug>AM_t#
map öt| <Plug>AM_t|
map öT~ <Plug>AM_T~
map öTsp <Plug>AM_Tsp
map öTab <Plug>AM_Tab
map öT@ <Plug>AM_T@
map öT? <Plug>AM_T?
map öT= <Plug>AM_T=
map öT< <Plug>AM_T<
map öT; <Plug>AM_T;
map öT: <Plug>AM_T:
map öTs, <Plug>AM_Ts,
map öT, <Plug>AM_T,o
map öT# <Plug>AM_T#
map öT| <Plug>AM_T|
map öHtd <Plug>AM_Htd
map öanum <Plug>AM_aunum
map öaunum <Plug>AM_aenum
map öafnc <Plug>AM_afnc
map öadef <Plug>AM_adef
map öadec <Plug>AM_adec
map öascom <Plug>AM_ascom
map öaocom <Plug>AM_aocom
map öadcom <Plug>AM_adcom
map öacom <Plug>AM_acom
map öabox <Plug>AM_abox
map öa( <Plug>AM_a(
map öa= <Plug>AM_a=
map öa< <Plug>AM_a<
map öa, <Plug>AM_a,
map öa? <Plug>AM_a?
nmap <silent> öslr :DBListVar
xmap <silent> ösa :DBVarRangeAssign
nmap <silent> ösap :'<,'>DBVarRangeAssign
nmap <silent> ösal :.,.DBVarRangeAssign
nmap <silent> ösas :1,$DBVarRangeAssign
nmap öso <Plug>DBOrientationToggle
nmap ösh <Plug>DBHistory
xmap <silent> östcl :exec "DBListColumn '".DB_getVisualBlock()."'"
nmap östcl <Plug>DBListColumn
nmap öslv <Plug>DBListView
nmap öslp <Plug>DBListProcedure
nmap öslt <Plug>DBListTable
xmap <silent> öslc :exec "DBListColumn '".DB_getVisualBlock()."'"
nmap öslc <Plug>DBListColumn
nmap ösbp <Plug>DBPromptForBufferParameters
nmap ösdpa <Plug>DBDescribeProcedureAskName
xmap <silent> ösdp :exec "DBDescribeProcedure '".DB_getVisualBlock()."'"
nmap ösdp <Plug>DBDescribeProcedure
nmap ösdta <Plug>DBDescribeTableAskName
xmap <silent> ösdt :exec "DBDescribeTable '".DB_getVisualBlock()."'"
nmap ösdt <Plug>DBDescribeTable
xmap <silent> ösT :exec "DBSelectFromTableTopX '".DB_getVisualBlock()."'"
nmap ösT <Plug>DBSelectFromTopXTable
nmap östa <Plug>DBSelectFromTableAskName
nmap östw <Plug>DBSelectFromTableWithWhere
xmap <silent> öst :exec "DBSelectFromTable '".DB_getVisualBlock()."'"
nmap öst <Plug>DBSelectFromTable
nmap <silent> ösep :'<,'>DBExecRangeSQL
nmap <silent> ösel :.,.DBExecRangeSQL
nmap <silent> ösea :1,$DBExecRangeSQL
nmap ösq <Plug>DBExecSQL
nmap ösE <Plug>DBExecSQLUnderTopXCursor
nmap öse <Plug>DBExecSQLUnderCursor
xmap ösE <Plug>DBExecVisualTopXSQL
xmap öse <Plug>DBExecVisualSQL
noremap ö_s :TCommentAs =&ft_
noremap ö_n :TCommentAs =&ft 
noremap ö_a :TCommentAs 
noremap ö_b :TCommentBlock
noremap <silent> ö_r :TCommentRight
xnoremap <silent> ö_i :TCommentInline
noremap ö_  :TComment 
noremap <silent> ö_p vip:TComment
xnoremap <silent> ö__ :TCommentMaybeInline
nnoremap <silent> ö__ :TComment
snoremap <silent> ö__ :TComment
onoremap <silent> ö__ :TComment
noremap ölf :CtrlP
noremap ölb :CtrlPBuffer
noremap öza zMzv
noremap ösp :call Splice()
noremap ötodo :vimgrep /\c\v(xxx:|todo:)/ % | cw
noremap ögd :Gdiff
noremap ögst :Gstatus
noremap ö>< mx:%s/> *</>\r</ggg=G'x
noremap ö,r mx:s/,/,\r/g='x
noremap öbl :e #
abbr functoin function
abbr .; .
abbr [9 []
abbr iint int
abbr itn int
abbr =! !=
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set autoread
set backspace=indent,eol,start
set backup
set backupdir=~/.backup/
set balloonexpr=eclim#util#Balloon(eclim#util#GetLineError(line('.')))
set completefunc=neocomplcache#complete#manual_complete
set completeopt=menuone,menu,longest
set cscopetag
set cscopeverbose
set display=truncate
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set guifont=Inconsolata\ Medium\ 11
set guioptions=aegirLt
set helplang=en
set hidden
set history=200
set hlsearch
set ignorecase
set incsearch
set langnoremap
set nolangremap
set laststatus=2
set listchars=tab:»·,nbsp:␣,trail:·
set nrformats=bin,hex
set ruler
set runtimepath=~/.vim/bundle/vundle,~/.vim/bundle/xmledit,~/.vim/bundle/ctrlp.vim,~/.vim/bundle/vim-fugitive,~/.vim/bundle/gundo.vim,~/.vim/bundle/histwin.vim,~/.vim/bundle/html-template-syntax,~/.vim/bundle/vim-javascript,~/.vim/bundle/vim-localrc,~/.vim/bundle/neocomplcache,~/.vim/bundle/nerdtree,~/.vim/bundle/vim-addon-mw-utils,~/.vim/bundle/tlib_vim,~/.vim/bundle/neosnippet,~/.vim/bundle/neosnippet-snippets,~/.vim/bundle/vim-snippets,~/.vim/bundle/vim-surround,~/.vim/bundle/vim-coffee-script,~/.vim/bundle/VimDebug,~/.vim/bundle/vimerl,~/.vim/bundle/tcomment_vim,~/.vim/bundle/tagbar,~/.vim/bundle/ghcmod-vim,~/.vim/bundle/vimproc,~/.vim/bundle/vim-clojure-static,~/.vim/bundle/dbext.vim,~/.vim/bundle/po.vim,~/.vim/bundle/sessionman.vim,~/.vim/bundle/slimv.vim,~/.vim/bundle/Vimchant,~/.vim/bundle/vim-colors-solarized,~/.vim,/nix/store/lricn677s04a1p2wxl33bm93gpfgvn2j-vim_configurable-8.0.0005/share/vim/vimfiles,/nix/store/lricn677s04a1p2wxl33bm93gpfgvn2j-vim_configurable-8.0.0005/share/vim/vim80,/nix/store/lricn677s04a1p2wxl33bm93gpfgvn2j-vim_configurable-8.0.0005/share/vim/vim80/pack/dist/opt/matchit,/nix/store/lricn677s04a1p2wxl33bm93gpfgvn2j-vim_configurable-8.0.0005/share/vim/vimfiles/after,~/.vim/after,~/.vim/bundle/vundle/,~/.vim/bundle/vundle/after,~/.vim/bundle/xmledit/after,~/.vim/bundle/ctrlp.vim/after,~/.vim/bundle/vim-fugitive/after,~/.vim/bundle/gundo.vim/after,~/.vim/bundle/histwin.vim/after,~/.vim/bundle/html-template-syntax/after,~/.vim/bundle/vim-javascript/after,~/.vim/bundle/vim-localrc/after,~/.vim/bundle/neocomplcache/after,~/.vim/bundle/nerdtree/after,~/.vim/bundle/vim-addon-mw-utils/after,~/.vim/bundle/tlib_vim/after,~/.vim/bundle/neosnippet/after,~/.vim/bundle/neosnippet-snippets/after,~/.vim/bundle/vim-snippets/after,~/.vim/bundle/vim-surround/after,~/.vim/bundle/vim-coffee-script/after,~/.vim/bundle/VimDebug/after,~/.vim/bundle/vimerl/after,~/.vim/bundle/tcomment_vim/after,~/.vim/bundle/tagbar/after,~/.vim/bundle/ghcmod-vim/after,~/.vim/bundle/vimproc/after,~/.vim/bundle/vim-clojure-static/after,~/.vim/bundle/dbext.vim/after,~/.vim/bundle/po.vim/after,~/.vim/bundle/sessionman.vim/after,~/.vim/bundle/slimv.vim/after,~/.vim/bundle/Vimchant/after,~/.vim/bundle/vim-colors-solarized/after,~/.vim/eclim,~/.vim/eclim/after
set scrolloff=5
set shiftwidth=4
set showcmd
set smartcase
set softtabstop=4
set statusline=%<\ %n:%f\ %m%r\ #%b\ %y%=%-35.(line:\ %l\ of\ %L,\ col:\ %c%V\ (%P)%)
set tags=./tags,./TAGS,tags,TAGS,~/.tdoc
set ttimeout
set ttimeoutlen=100
set undodir=~/.vim-undos/
set undofile
set wildignore=*/.git/*,*/target/*,*.REMOTE.*,*.BACKUP.*,*.LOCAL.*,*.BASE.*,*.log.*,*.hi,*.o
set wildmenu
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/programming/wtfboths
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +0 ~/.vimrc
argglobal
silent! argdel *
argadd ~/.vimrc
edit ~/.vimrc
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
vnoremap <buffer> <silent> [" :exe "normal! gv"|call search('\%(^\s*".*\n\)\%(^\s*"\)\@!', "bW")
nnoremap <buffer> <silent> [" :call search('\%(^\s*".*\n\)\%(^\s*"\)\@!', "bW")
vnoremap <buffer> <silent> [] m':exe "normal! gv"|call search('^\s*endf*\%[unction]\>', "bW")
nnoremap <buffer> <silent> [] m':call search('^\s*endf*\%[unction]\>', "bW")
vnoremap <buffer> <silent> [[ m':exe "normal! gv"|call search('^\s*fu\%[nction]\>', "bW")
nnoremap <buffer> <silent> [[ m':call search('^\s*fu\%[nction]\>', "bW")
vnoremap <buffer> <silent> ]" :exe "normal! gv"|call search('^\(\s*".*\n\)\@<!\(\s*"\)', "W")
nnoremap <buffer> <silent> ]" :call search('^\(\s*".*\n\)\@<!\(\s*"\)', "W")
vnoremap <buffer> <silent> ][ m':exe "normal! gv"|call search('^\s*endf*\%[unction]\>', "W")
nnoremap <buffer> <silent> ][ m':call search('^\s*endf*\%[unction]\>', "W")
vnoremap <buffer> <silent> ]] m':exe "normal! gv"|call search('^\s*fu\%[nction]\>', "W")
nnoremap <buffer> <silent> ]] m':call search('^\s*fu\%[nction]\>', "W")
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal backupcopy=
setlocal balloonexpr=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=sO:\"\ -,mO:\"\ \ ,eO:\"\",:\"
setlocal commentstring=\"%s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=neocomplcache#complete#manual_complete
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'vim'
setlocal filetype=vim
endif
setlocal fixendofline
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
set foldmethod=syntax
setlocal foldmethod=syntax
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=croql
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=0
setlocal include=
setlocal includeexpr=
setlocal indentexpr=GetVimIndent()
setlocal indentkeys=0{,0},:,0#,!^F,o,O,e,=end,=else,=cat,=fina,=END,0\\
setlocal noinfercase
setlocal iskeyword=@,48-57,_,192-255,#
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
set list
setlocal list
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=bin,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
set relativenumber
setlocal relativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=4
setlocal noshortname
setlocal signcolumn=auto
setlocal nosmartindent
setlocal softtabstop=4
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'vim'
setlocal syntax=vim
endif
setlocal tabstop=8
setlocal tagcase=
setlocal tags=
setlocal textwidth=78
setlocal thesaurus=
setlocal undofile
setlocal undolevels=-123456
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
let s:l = 61 - ((44 * winheight(0) + 25) / 50)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
61
normal! 018|
tabnext 1
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToO
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
