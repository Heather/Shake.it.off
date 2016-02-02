@echo off
cabal install --force-reinstall
shake -f
pause
