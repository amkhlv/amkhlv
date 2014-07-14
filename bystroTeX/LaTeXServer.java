/**
 * Created with IntelliJ IDEA.
 * User: andrei
 * Date: 16/10/13
 * Time: 10:47
 * To change this template use File | Settings | File Templates.
 */


   /*
 * Copyright 2012,2013 Andrei Mikhailov
 *
 * This file is part of bystroTeX.
 *
 * bystroTeX is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * bystroTeX is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with bystroTeX.  If not, see <http://www.gnu.org/licenses/>.
 */

import org.scilab.forge.jlatexmath.ParseException;
import org.scilab.forge.jlatexmath.TeXConstants;
import org.scilab.forge.jlatexmath.TeXFormula;
import org.scilab.forge.jlatexmath.TeXIcon;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import javax.xml.parsers.*;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import java.awt.Color;

/**
 * Created with IntelliJ IDEA.
 * User: andrei
 * Date: 29/06/13
 * Time: 15:10
 * <p/>
 * Usage:
 * java -cp jlatexmath.jar LaTeXServer pipename.fifo  logfilename.txt
 * The program receives the description of the formula in the XML format on the pipe pipename.fifo
 * The XML contains various parameters of the formula, including the LaTeX code and the filename where to save png
 * In return, it writes to the same pipename.fifo the XML file containing at least the ``depth'' node, which
 * encodes the formula offset. It also saves the resulting png in the file whose name was provided in the input XML.
 */
public class LaTeXServer {

    public static FileReader rcv(String pipename) {
        FileReader inputStream = null;
        try {
            inputStream = new FileReader(pipename);
        } catch (FileNotFoundException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
        return inputStream;
    }


    public static void snd(String s, String pipename) {
        FileWriter outputStream = null;
        try {
            outputStream = new FileWriter(pipename);
        } catch (IOException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
        try {
            // System.out.println("======================== WAITING JUST SENT DATA");
            outputStream.write(s);
            outputStream.close();
            // System.out.println("======================== CLOSED PIPE");
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public static Document processLaTeX(String tex, Integer sz, String fname, java.awt.Color bg, java.awt.Color fg) {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = null;
        try {
            builder = factory.newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        Document domOut = builder.newDocument();
        try {
            TeXFormula formula = new TeXFormula(tex);
            TeXIcon ti = formula.createTeXIcon(TeXConstants.STYLE_DISPLAY, sz);
            int height = ti.getIconHeight();
            int newdepth = ti.getIconDepth();
            /*
            if ((2 * height) > (3 * sz)) {
                newdepth = newdepth + 10;  // this is a hack around the jlatexmath bug
                formula = formula.addStrut(TeXConstants.UNIT_PIXEL, 4, 1, newdepth);
            }
            */
            //            formula.createPNG(TeXConstants.STYLE_DISPLAY, sz, fname, java.awt.Color.WHITE, java.awt.Color.BLACK);
            formula.createPNG(TeXConstants.STYLE_DISPLAY, sz, fname, bg, fg);
            Element root = domOut.createElement("report");
            domOut.appendChild(root);
            Element depthElem = domOut.createElement("depth");
            root.appendChild(depthElem);
            depthElem.appendChild(domOut.createTextNode(Integer.toString(newdepth)));
            return domOut;
        } catch (ParseException e) {
            Element root = domOut.createElement("report");
            domOut.appendChild(root);
            Element errorElem = domOut.createElement("error");
            root.appendChild(errorElem);
            errorElem.appendChild(domOut.createTextNode(e.getMessage()));
            return domOut;
        }
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        Boolean doMainLoop = true;
        FileWriter log = null;
        try {
            log = new FileWriter(args[1]);
            log.write("Starting LaTeXServer\n");
            log.flush();
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        while (doMainLoop) {
            FileReader inputStream = rcv(args[0]);
            BufferedReader br = new BufferedReader(inputStream);
            String line1;
            try {
                line1 = br.readLine();
                log.write("--------------------------\n" + line1 + "\n");
                log.flush();
            } catch (IOException e) {
                // TODO Auto-generated catch block
                line1 = "ERROR";
                e.printStackTrace();
            }
            String sz_str;
            try {
                if (line1.equals("HI")) {
                    try {
                        inputStream.close();
                    } catch (IOException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                    snd("LISTENING", args[0]);
                } else if (line1.equals("STOP")) {
                    doMainLoop = false;
                    try {
                        inputStream.close();
                    } catch (IOException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                } else if (line1.equals("XML")) {
                    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                    DocumentBuilder builder = null;
                    try {
                        builder = factory.newDocumentBuilder();
                    } catch (ParserConfigurationException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                    }
                    InputSource is = new InputSource(br);
                    try {
                        Document dom = builder.parse(is);
                        Element formula = dom.getDocumentElement();
                        Integer sz = Integer.parseInt(formula.getAttribute("size"));
                        String fn = formula.getAttribute("filename");
                        String fgcol = formula.getAttribute("fg");
                        String bgcol = formula.getAttribute("bg");
                        String tex = formula.getTextContent();
                        log.write(fn + " \n" + tex + "\nsize:" + formula.getAttribute("size") + "\n");
                        log.flush();
                        inputStream.close();
                        String[] bgcols = bgcol.split(":");
                        String[] fgcols = fgcol.split(":");
                        java.awt.Color bg = new Color(Integer.parseInt(bgcols[0]),
                                Integer.parseInt(bgcols[1]),
                                Integer.parseInt(bgcols[2]));
                        java.awt.Color fg = new Color(Integer.parseInt(fgcols[0]),
                                Integer.parseInt(fgcols[1]),
                                Integer.parseInt(fgcols[2]));
                        Document domOut = processLaTeX(tex, sz, fn, bg, fg);
                        FileWriter outputStream = new FileWriter(args[0]);
                        Transformer transformer = null;
                        Transformer logTrans = null;
                        try {
                            transformer = TransformerFactory.newInstance().newTransformer();
                            logTrans = TransformerFactory.newInstance().newTransformer();
                        } catch (TransformerConfigurationException e) {
                            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                        }
                        StreamResult strRes = new StreamResult(outputStream);
                        StreamResult strLog = new StreamResult(log);
                        DOMSource source = new DOMSource(domOut);
                        try {
                            transformer.transform(source, strRes);
                            outputStream.flush();
                            outputStream.close();
                            logTrans.transform(source, strLog);
                            log.flush();
                        } catch (TransformerException e) {
                            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                        }
                    } catch (SAXException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                    }
                } else {
                    try {
                        inputStream.close();
                    } catch (IOException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                    doMainLoop = false;
                    System.err.println("*** ERROR: unknown command received by LaTeXServer ***");
                }
                log.write("========\n");
                log.flush();
            } catch (NumberFormatException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        try {
            log.close();
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }

}

