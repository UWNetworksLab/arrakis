/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Linq;
using System.Windows;

namespace Aquarium
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application
    {
        public App()
            : base()
        {
            if (!System.Diagnostics.Debugger.IsAttached)
                this.DispatcherUnhandledException +=
                    new System.Windows.Threading.DispatcherUnhandledExceptionEventHandler(App_DispatcherUnhandledException);

            this.Startup += new StartupEventHandler(App_Startup);
        }

        public string[] CommandLineArguments;

        void App_Startup(object sender, StartupEventArgs e)
        {
            this.CommandLineArguments = e.Args;
        }

        void App_DispatcherUnhandledException(object sender, System.Windows.Threading.DispatcherUnhandledExceptionEventArgs e)
        {
            Exception ex = e.Exception;
            MessageBox.Show(ex.ToString(), "Application Unhandled Exception",
                MessageBoxButton.OK, MessageBoxImage.Error);
            this.Shutdown(255);
            e.Handled = true;
        }
    }
}
