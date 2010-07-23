function! BDD(args)
 if bufname("%") =~ "test.rb"
   call RunTest(a:args)
 elseif bufname("%") =~ ".scala"
   call RunSBTTest()
 elseif bufname("%") =~ "spec.rb"
   call RunSpec(a:args)
 else
   echo "don't know how to BDD this file"
 end
endfunction
 
function! RunTest(args)
  let cursor = matchstr(a:args, '\d\+')
  if cursor
    while !exists("cmd") && cursor != 1
      if match(getline(cursor), 'def test') >= 0
        let cmd = ":! ruby % -vv -n ". matchstr(getline(cursor), "test_[a-zA-Z_]*")
      else
        let cursor -= 1
      end
    endwhile
  end
  if !exists("cmd")
    let cmd = ":! ruby % -vv"
  end
  execute cmd
endfunction
 
function! RunSpec(args)
  if exists("b:rails_root") && filereadable(b:rails_root . "/script/spec")
    let spec = b:rails_root . "/script/spec"
  else
    let spec = "spec"
  end
  let cmd = ":! " . spec . " % -cfn " . a:args
  execute cmd
endfunction
 
function! RunSBTTest()
  execute ":! java -jar ~/sbt-launcher-0.5.5.jar test"
endfunction
 
map !s :call BDD("-l " . <C-r>=line('.')<CR>)
map !S :call BDD("")

