ReplaceSpecialCharacter <- function(dirtyData, case="none")
{
  dirtyChar <- "szþàáâãäåçèéêëìíîïðñòóôõöùúûüýSZÞÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖÙÚÛÜÝ";
  clearChar <- "szyaaaaaaceeeeiiiidnooooouuuuyszyaaaaaaceeeeiiiidnooooouuuuy";
  
  cleanData <- chartr(old=dirtyChar, new=clearChar, x=dirtyData);
  
  if(case=="upper"){
    cleanData <- toupper(cleanData);
  }else if(case=="lower"){
    cleanData <- tolower(cleanData);
  }
  
  return(cleanData);
}
