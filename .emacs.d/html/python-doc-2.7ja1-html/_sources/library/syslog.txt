
:mod:`syslog` --- Unix syslog ライブラリルーチン群
==================================================

.. module:: syslog
   :platform: Unix
   :synopsis: Unix syslog ライブラリルーチン群へのインタフェース。


このモジュールでは Unix ``syslog`` ライブラリルーチン群へのインタフェースを提供します。
``syslog`` の便宜レベルに関する詳細な記述は Unix マニュアルページを参照してください。

このモジュールはシステムの ``syslog`` ファミリのルーチンをラップしています。
syslog サーバーと通信できる pure Python のライブラリが、
:mod:`logging.handlers` モジュールの :class:`SysLogHandler` にあります。

このモジュールでは以下の関数を定義しています:


.. function:: syslog([priority,] message)

   文字列 *message* をシステムログ機構に送信します。
   末尾の改行文字は必要に応じて追加されます。
   各メッセージは *facility* および *level* からなる優先度でタグ付けされます。
   オプションの *priority* 引数はメッセージの優先度を定義します。
   標準の値は :const:`LOG_INFO` です。
   *priority* 中に、便宜レベルが  (``LOG_INFO | LOG_USER`` のように)
   論理和を使ってコード化されていない場合、 :func:`openlog` を呼び出した際の値が使われます。

   :func:`syslog` が呼び出される前に :func:`openlog` が呼び出されなかった場合、
   ``openlog()`` が引数なしで呼び出されます。

.. function:: openlog([ident[, logopt[, facility]]])

   :func:`openlog` 関数を呼び出すことで以降の :func:`syslog` の呼び出しに対する
   ログオプションを設定することができます。
   ログがまだ開かれていない状態で :func:`syslog` を呼び出すと :func:`openlog`
   が引数なしで呼び出されます。

   オプションの *ident* キーワード引数は全てのメッセージの先頭に付く文字列で、
   デフォルトでは ``sys.argv[0]`` から前方のパス部分を取り除いたものです。

   オプションの *logopt* キーワード引数 (デフォルトは 0) はビットフィールドです。
   組み合わせられる値については下記を参照してください。

   オプションの *facility* キーワード引数 (デフォルトは :const:`LOG_USER`) は
   明示的に facility が encode されていないメッセージに設定される facility です。


.. function:: closelog()

   syslog モジュールの値をリセットし、システムライブラリの ``closelog()`` を呼び出します。

   この関数を呼ぶと、モジュールが最初に import されたときと同じようにふるまいます。
   例えば、(:func:`openlog` を呼び出さないで) :func:`syslog` を最初に呼び出したときに、
   :func:`openlog` が呼び出され、 *ident* やその他の :func:`openlog` の引数は
   デフォルト値にリセットされます。

.. function:: setlogmask(maskpri)

   優先度マスクを *maskpri* に設定し、以前のマスク値を返します。
   *maskpri* に設定されていない優先度レベルを持った
   :func:`syslog` の呼び出しは無視されます。標準では全ての優先度をログ出力します。
   関数 ``LOG_MASK(pri)`` は個々の優先度
   *pri* に対する優先度マスクを計算します。関数 ``LOG_UPTO(pri)`` は優先度 *pri*
   までの全ての優先度を含むようなマスクを計算します。

このモジュールでは以下の定数を定義しています:

優先度 (高い優先度順):
   :const:`LOG_EMERG` 、 :const:`LOG_ALERT` 、 :const:`LOG_CRIT` 、 :const:`LOG_ERR` 、
   :const:`LOG_WARNING` 、 :const:`LOG_NOTICE` 、 :const:`LOG_INFO` 、
   :const:`LOG_DEBUG` 。

便宜レベル:
   :const:`LOG_KERN` 、 :const:`LOG_USER` 、 :const:`LOG_MAIL` 、 :const:`LOG_DAEMON` 、
   :const:`LOG_AUTH` 、 :const:`LOG_LPR` 、 :const:`LOG_NEWS` 、 :const:`LOG_UUCP` 、
   :const:`LOG_CRON` 、および :const:`LOG_LOCAL0` から :const:`LOG_LOCAL7` 。

ログオプション:
   ``<syslog.h>`` で定義されている場合、 :const:`LOG_PID` 、 :const:`LOG_CONS` 、
   :const:`LOG_NDELAY` 、 :const:`LOG_NOWAIT` 、および :const:`LOG_PERROR` 。

例
--------

シンプルな例
~~~~~~~~~~~~~~

1つ目のシンプルな例::

   import syslog

   syslog.syslog('Processing started')
   if error:
       syslog.syslog(syslog.LOG_ERR, 'Processing started')

いくつかのログオプションを設定する例。ログメッセージにプロセスIDを含み、
メッセージをメールのログ用の facility にメッセージを書きます。 ::

   syslog.openlog(logopt=syslog.LOG_PID, facility=syslog.LOG_MAIL)
   syslog.syslog('E-mail processing initiated...')
