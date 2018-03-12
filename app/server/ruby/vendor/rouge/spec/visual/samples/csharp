////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// MIT X11 license, Copyright (c) 2005-2006 by:                               //
//                                                                            //
// Authors:                                                                   //
//      Michael Dominic K. <michaldominik@gmail.com>                          //
//                                                                            //
// Permission is hereby granted, free of charge, to any person obtaining a    //
// copy of this software and associated documentation files (the "Software"), //
// to deal in the Software without restriction, including without limitation  //
// the rights to use, copy, modify, merge, publish, distribute, sublicense,   //
// and/or sell copies of the Software, and to permit persons to whom the      //
// Software is furnished to do so, subject to the following conditions:       //
//                                                                            //
// The above copyright notice and this permission notice shall be included    //
// in all copies or substantial portions of the Software.                     //
//                                                                            //
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS    //
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                 //
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN  //
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,   //
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR      //
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE  //
// USE OR OTHER DEALINGS IN THE SOFTWARE.                                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

namespace Diva.Core {
        
        using System;
        using Widgets;
        using System.Xml;
        using Util;
        using System.Collections.Generic;
        using System.Collections;
        using Basics;

        public class OpenerTask : Task, IBoilProvider {

                // Private structs ////////////////////////////////////////////
                
                struct ObjectInfo {

                        public ObjectContainer Container;
                        public int[] Depends;
                        public string SystemType;
                        public int RefId;
                        
                        /* CONSTRUCTOR */
                        public ObjectInfo (ObjectContainer container)
                        {
                                Container = container;
                                Depends = container.Depends.ToArray ();
                                SystemType = container.SystemType;
                                RefId = container.RefId;
                        }
                        
                        public override string ToString ()
                        {
                                return String.Format ("Type: {0} Deps count: {1} Id: {2}",
                                                      SystemType, Depends.Length, RefId);
                        }
                        
                        public bool IsUnBoilable (IBoilProvider provider)
                        {
                                if (Depends.Length == 0)
                                        return true;
                                
                                foreach (int id in Depends)
                                        if (! (provider.Contains (id)))
                                                return false;
                                
                                return true;
                        }
                        
                }
                                 
                // Enums //////////////////////////////////////////////////////
                
                enum OpenerTaskStep { Init, Header, ProjectInfoRead, ObjectListRead,
                                      ObjectListParse, ObjectListUnBoil, FindRoots,
                                      Finished };
                
                // Fields /////////////////////////////////////////////////////
                
                string fileName;                         // Filename we're reading
                XmlDocument xmlDocument;                 // Our document
                //XmlNode projectInfoNode;               // <projectinfo> node
                IEnumerator objectsEnumerator;           // Enumerator
                List <ObjectInfo> objectsList;           // Objects list
                ObjectListContainer objectListContainer;
                OpenerTaskStep currentStep;              // Our current step
                
                Dictionary <int, object> idToObject;     // Id -> object
                Dictionary <object, int> objectToId;     // Object -> Id

                string projectName = String.Empty;
                string projectDirectory = String.Empty;
                TagList projectTagList;
                StuffList projectStuffList;
                TrackList projectTrackList;
                ClipList projectClipList;
                MediaItemList projectMediaItemList;
                Commander projectCommander;
                Gdv.Pipeline projectPipeline;
                Gdv.ProjectFormat projectFormat;
                
                // Properties /////////////////////////////////////////////////
                
                public string ProjectName {
                        get { return projectName; }
                }
                
                public string ProjectDirectory {
                        get { return projectDirectory; }
                }
                
                public TagList ProjectTagList {
                        get { return projectTagList; }
                }
                
                public StuffList ProjectStuffList {
                        get { return projectStuffList; }
                }
                
                public TrackList ProjectTrackList {
                        get { return projectTrackList; }
                }

                public ClipList ProjectClipList {
                        get { return projectClipList; }
                }
                
                public MediaItemList ProjectMediaItemList {
                        get { return projectMediaItemList; }
                }
                
                public Commander ProjectCommander {
                        get { return projectCommander; }
                }
                
                public Gdv.Pipeline ProjectPipeline {
                        get { return projectPipeline; }
                }

                public Gdv.ProjectFormat ProjectFormat {
                        get { return projectFormat; }
                }

                // Public methods /////////////////////////////////////////////
                
                /* CONSTRUCTOR */
                public OpenerTask (string fileName)
                {
                        this.fileName = fileName;
                }
                
                public override void Reset ()
                {
                        objectToId = new Dictionary <object, int> ();
                        idToObject = new Dictionary <int, object> ();
                        
                        xmlDocument = null;
                        //projectInfoNode = null;
                        
                        currentStep = OpenerTaskStep.Init;
                        
                        base.Reset ();
                }
                
                public int GetIdForObject (object o)
                {
                        return objectToId [o];
                }
                
                public object GetObjectForId (int id)
                {
                        return idToObject [id];
                }
                
                public bool Contains (int id)
                {
                        return idToObject.ContainsKey (id);
                }
                
                // Private methods ////////////////////////////////////////////
                
                protected override TaskStatus ExecuteStep (int s)
                {
                        bool cont = true;
                        
                        // Main
                        switch (currentStep) {
                                        
                                case OpenerTaskStep.Init:
                                        objectsList = new List <ObjectInfo> ();
                                        xmlDocument = new XmlDocument ();
                                        xmlDocument.Load (fileName);
                                        currentStep = OpenerTaskStep.Header;
                                        break;
                                        
                                case OpenerTaskStep.Header:
                                        //ReadHeader ();
                                        currentStep = OpenerTaskStep.ProjectInfoRead;
                                        break;

                                case OpenerTaskStep.ProjectInfoRead:
                                        foreach (XmlNode node in xmlDocument.DocumentElement.ChildNodes)
                                                if (node.Name == "projectinfo") 
                                                        ResolveProjectInfoNode (node);

                                        // FIXME: Fail if not found/not resolved
                                        currentStep = OpenerTaskStep.ObjectListRead;
                                        break;
                                        
                                case OpenerTaskStep.ObjectListRead:
                                        foreach (XmlNode node in xmlDocument.DocumentElement.ChildNodes)
                                                if (node.Name == "objectlist") 
                                                        objectListContainer = (ObjectListContainer)
                                                                DataFactory.MakeDataElement  (node as XmlElement);
                                                        
                                        if (objectListContainer == null)
                                                throw new Exception ("ObjectListContainer not found!");

                                        currentStep = OpenerTaskStep.ObjectListParse;
                                        break;

                                case OpenerTaskStep.ObjectListParse:
                                        bool flush = EnumerateSomeObjects ();
                                        if (flush)
                                                currentStep = OpenerTaskStep.ObjectListUnBoil;
                                        break;

                                case OpenerTaskStep.ObjectListUnBoil:
                                        bool done = UnBoilSomeObjects ();
                                        if (done)
                                                currentStep = OpenerTaskStep.FindRoots;
                                        break;
                                        
                                        
                                case OpenerTaskStep.FindRoots:
                                        projectTrackList = (TrackList) FindRoot ("tracklist");
                                        projectTagList = (TagList) FindRoot ("taglist");
                                        projectStuffList = (StuffList) FindRoot ("stufflist");
                                        projectClipList = (ClipList) FindRoot ("cliplist");
                                        projectMediaItemList = (MediaItemList) FindRoot ("mediaitemlist");
                                        projectPipeline = (Gdv.Pipeline) FindRoot ("pipeline");
                                        projectCommander = (Commander) FindRoot ("commander");
                                        projectFormat = (Gdv.ProjectFormat) FindRoot ("projectformat");
                                        
                                        currentStep = OpenerTaskStep.Finished;
                                        break;
                                        
                                case OpenerTaskStep.Finished:
                                        cont = false;
                                        break;
                                        
                                default:
                                        break;
                        }
                                                
                        // Post 
                        if (cont) 
                                return TaskStatus.Running;
                        else
                                return TaskStatus.Done;
                }

                /*
                void ReadHeader ()
                {
                        // FIXME: Read all the attributes from the <divaproject> element
                        }*/

                void ResolveProjectInfoNode (XmlNode node)
                {
                        foreach (XmlNode childNode in node) {
                                
                                switch (childNode.Name) {
                                        
                                        case "name":
                                                projectName = childNode.FirstChild.Value;
                                                break;
                                        
                                        case "directory":
                                                projectDirectory = childNode.FirstChild.Value;
                                                break;

                                                // FIXME: Duration etc.
                                }
                        }
                }
                
                bool EnumerateSomeObjects ()
                {
                        if (objectsEnumerator == null)
                                objectsEnumerator = objectListContainer.FindAllObjects ().GetEnumerator ();
                        
                        for (int i = 0; i < 10; i++) {
                                if (objectsEnumerator.MoveNext () == false)
                                        return true;

                                ObjectContainer container = (ObjectContainer)
                                        objectsEnumerator.Current;
                                
                                ObjectInfo newInfo = new ObjectInfo (container);
                                objectsList.Add (newInfo);
                        }
                        
                        return false;
                }

                ObjectInfo GetNextCandidate ()
                {
                        foreach (ObjectInfo objInfo in objectsList)
                                if (objInfo.IsUnBoilable (this))
                                        return objInfo;
                        
                        throw new Exception ("FIXME: No more unboilable objects found. Recursive?");
                }
                
                bool UnBoilSomeObjects ()
                {
                        for (int i = 0; i < 5; i++) {
                                // All unboiled
                                if (objectsList.Count == 0)
                                        return true;
                                
                                ObjectInfo objInfo = GetNextCandidate ();

                                object o = BoilFactory.UnBoil (objInfo.Container, this);
                                objectsList.Remove (objInfo);

                                // Add
                                idToObject [objInfo.RefId] = o;
                                objectToId [o] = objInfo.RefId;

                        }
                        
                        return false;
                }

                object FindRoot (string rootString)
                {
                        ObjectContainer container = objectListContainer.FindObjectContainer (rootString);
                        return idToObject [container.RefId];
                }
                
        }
        
}
