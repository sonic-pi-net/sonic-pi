#include <QProxyStyle>

class BorderlessLinksProxyStyle : public QProxyStyle
{
public:
  int styleHint(StyleHint hint, const QStyleOption *option, const QWidget *widget, QStyleHintReturn *returnData) const
  {
    if (hint == SH_TextControl_FocusIndicatorTextCharFormat)
      return false;
    return QProxyStyle::styleHint(hint, option, widget, returnData);
  }
};
