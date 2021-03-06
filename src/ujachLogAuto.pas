{ ******************************************************** }
{ **                                                    ** }
{ ** Multithreaded Log for Delphi                       ** }
{ **                                                    ** }
{ ** Author:                                            ** }
{ **     Juan Antonio Castillo H. (jachguate)           ** }
{ **                                                    ** }
{ ** Copyright (c) 2007-2022                            ** }
{ **                                                    ** }
{ ** https://github.com/jachguate/jachLogMgr            ** }
{ **                                                    ** }
{ ** Available under MIT License                        ** }
{ ******************************************************** }

{
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
}

unit ujachLogAuto;

interface

uses
  UjachLogMgr, ujachLogToDisk, ujachLogToConsole;

  function GetLogDiskWriter: TjachLogToDisk;
  function GetLogConsoleWriter: TjachLogToConsole;

implementation

var
  lLogToDisk: TjachLogToDisk;
  lLogToConsole: TjachLogToConsole;

function GetLogDiskWriter: TjachLogToDisk;
begin
  Result := lLogToDisk;
end;

function GetLogConsoleWriter: TjachLogToConsole;
begin
  Result := lLogToConsole;
end;

initialization
  jachLog := TjachLog.Create();
  lLogToDisk := TjachLogToDisk.Create;
  jachLog.RegisterLogWriter(lLogToDisk);
  if IsConsole then
  begin
    lLogToConsole := TjachLogToConsole.Create;
    jachLog.RegisterLogWriter(lLogToConsole);
  end;
finalization
end.
