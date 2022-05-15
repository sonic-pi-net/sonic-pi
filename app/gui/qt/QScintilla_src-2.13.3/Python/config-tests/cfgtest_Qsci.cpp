#include <QCoreApplication>
#include <QFile>
#include <QTextStream>

#include <Qsci/qsciglobal.h>


int main(int argc, char **argv)
{
    QCoreApplication app(argc, argv);
    QFile outf(argv[1]);

    if (!outf.open(QIODevice::WriteOnly|QIODevice::Truncate|QIODevice::Text))
        return 1;

    QTextStream out(&outf);

    out << QSCINTILLA_VERSION << '\n';
    out << QSCINTILLA_VERSION_STR << '\n';

    return 0;
}
