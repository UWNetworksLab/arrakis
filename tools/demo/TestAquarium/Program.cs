/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

﻿using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using System.Net;
using System.Net.Sockets;
using System.IO;

// For now this just sends a single file back, but later we will use this
// to test and insert error conditions such as broken connection and so on
// in order to robustify the GUI.

namespace TestAquarium
{
    class Program
    {
        /// <summary>
        /// Reads the trace command from the stream
        /// </summary>
        /// <returns>true if got trace command, false for error or EOF</returns>
        private static bool ReadTraceCommand(NetworkStream stream)
        {
            byte[] trace = new byte[6];
            int offset = 0;
            do
            {
                int got = stream.Read(trace, offset, trace.Length - offset);
                if (got == 0)
                    return false;
                offset += got;
            } while (offset < trace.Length);
            if (trace[0] == (byte)'t' && trace[1] == (byte)'r' && trace[2] == (byte)'a'
                && trace[3] == (byte)'c' && trace[4] == (byte)'e' && trace[5] == (byte)'\n')
                return true;
            return false;
        }

        static int Body(string[] args)
        {
            if (args.Length != 1)
                return Usage();

            string dir, files;
            if (Directory.Exists(args[0]))
            {
                dir = args[0];
                files = "*.log";
            }
            else
            {
                dir = Path.GetDirectoryName(args[0]);
                if (string.IsNullOrEmpty(dir)) dir = Environment.CurrentDirectory;
                files = Path.GetFileName(args[0]);
            }

            string[] filenames = Directory.GetFiles(dir, files);

            byte[][] datas = new byte[filenames.Length][];

            for (int i = 0; i < filenames.Length; i++)
                datas[i] = File.ReadAllBytes(filenames[i]);

            Random rand = new Random();

            TcpListener listener = new TcpListener(IPAddress.Any, 666);
            listener.Start();

            while (true)
            {
                Console.WriteLine("Waiting for a connection...");
                TcpClient client = listener.AcceptTcpClient();
                Console.WriteLine("Connected from " + client.Client.RemoteEndPoint);
                NetworkStream stream = client.GetStream();
                int reqno = 0;

                try
                {
                    int previndex = datas.Length;
                    while (ReadTraceCommand(stream))
                    {
                        int index = 0;
                        if (datas.Length > 1)
                        {
                            // Pick a random one, but not the same as last time
                            index = rand.Next(previndex < datas.Length ? datas.Length - 1 : datas.Length);
                            if (index >= previndex)
                                index++;
                            if (index == previndex)
                                throw new NotImplementedException();
                            previndex = index;
                        }

                        byte[] data = datas[index];

                        Console.WriteLine("Got request {0} sending data {1}", reqno++, filenames[index]);
                        string lenstr = String.Format("{0,6}", data.Length);
                        if (lenstr.Length != 6)
                            throw new InvalidOperationException();
                        byte[] lendata = Encoding.ASCII.GetBytes(lenstr);

                        stream.Write(lendata, 0, lendata.Length);
                        stream.Write(data, 0, data.Length);
                    }

                    stream.Close();
                    client.Close();
                }
                catch (IOException ex)
                {
                    Console.WriteLine("Peer went away:" + ex.Message);
                    stream.Dispose();
                    client.Close();
                }
            }
        } // Body


        public static int Usage(string error)
        {
            TextWriter wr = Console.Error;
            wr.WriteLine("usage: TestAquarium <filename or glob>");
            wr.WriteLine();
            if (!string.IsNullOrEmpty(error))
                wr.WriteLine("*** error: " + error);
            return 1;
        }
        public static int Usage()
        {
            return Usage(null);
        }


        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        static int Main(string[] args)
        {
            if (System.Diagnostics.Debugger.IsAttached)
                return Body(args);
            else
            {
                try
                {
                    return Body(args);
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("An exception occurred: {0}", ex.ToString());
                    return 2;
                }
            }
        } // Main
    } // class Program
} // namespace
