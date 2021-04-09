" toggle between absolute and relative line numbers
function! NumberToggle()
    if(&relativenumber == 1)
        set norelativenumber number
    else
        set number relativenumber
    endif
endfunc
