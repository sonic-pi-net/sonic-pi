//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, distribution,
// and distribution of modified versions of this work as long as this
// notice is included.
//++


#include  <Qsci/qsciabstractapis.h>

class SonicPiAPIs : public QsciAbstractAPIs
{
 public:
  static const int Func = 0, FX = 1, Synth = 2, Sample = 3, NContext = 4;

  SonicPiAPIs(QsciLexer *lexer, QString sample_path);

  void addSymbol(int context, QString sym);
  void addKeyword(int context, QString keyword);

  //! Destroy the QsciAPIs instance.
  virtual ~SonicPiAPIs();
  
  //! \reimp
  virtual void updateAutoCompletionList(const QStringList &context,
					QStringList &list);
  
  //! \reimp
  virtual void autoCompletionSelected(const QString &sel);
  
  //! \reimp
  virtual QStringList callTips(const QStringList &context, int commas,
			       QsciScintilla::CallTipsStyle style,
			       QList<int> &shifts);
  
  //! \internal Reimplemented to receive termination events from the worker
  //! thread.
  virtual bool event(QEvent *e);
  

 private:
  QStringList keywords[NContext];
};
