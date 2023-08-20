Imports System.IO
Imports System.Windows.Forms

Public Class Node
    Public Property Name As String
    Public Property States As List(Of String)
    Public Property Parents As List(Of Node)
    Public Property CPT As ConditionalProbabilityTable

    Public Sub New(name As String, states As List(Of String))
        Me.Name = name
        Me.States = states
        Parents = New List(Of Node)()
    End Sub
End Class
Public Class ConditionalProbabilityTable
    Public Property Node As Node
    Public Property Values As Dictionary(Of List(Of String), Double)

    Public Sub New(node As Node)
        Me.Node = node
        Values = New Dictionary(Of List(Of String), Double)()
    End Sub

    Public Sub SetEntry(parentStates As List(Of String), value As Double)
        Values(parentStates) = value
    End Sub
End Class
Public Class InferenceEngine
    Public Sub New(network As BeliefNetwork)
        Me.Network = network
    End Sub

    Public Property Network As BeliefNetwork

    Public Function CalculateConditionalProbability(node As Node, state As String) As Double
        Dim totalProbability As Double = 0.0
        Dim parentNodes = node.Parents

        For Each parentState In CartesianProduct(parentNodes.Select(Function(n) n.States))
            Dim evidence As New Dictionary(Of Node, String)()
            For i = 0 To parentNodes.Count - 1
                evidence(parentNodes(i)) = parentState(i)
            Next

            Dim jointProbability As Double = CalculateJointProbability(evidence)
            totalProbability += jointProbability
        Next

        Dim evidenceWithState As New Dictionary(Of Node, String)()
        evidenceWithState(node) = state

        Dim conditionalProbability = CalculateJointProbability(evidenceWithState) / totalProbability
        Return conditionalProbability
    End Function

    Private Function CalculateJointProbability(evidence As Dictionary(Of Node, String)) As Double
        Dim jointProbability As Double = 1.0

        For Each node In Network.Nodes
            Dim nodeProbability As Double

            If evidence.ContainsKey(node) Then
                Dim parentStates = node.Parents.Select(Function(parent) evidence(parent))
                nodeProbability = node.CPT.Values(parentStates.ToList())
            Else
                Dim parentStates = node.Parents.Select(Function(parent) evidence(parent))
                nodeProbability = node.CPT.Values(parentStates.ToList())
            End If

            jointProbability *= nodeProbability
        Next

        Return jointProbability
    End Function

    Private Iterator Function CartesianProduct(sequences As IEnumerable(Of IEnumerable(Of String))) As IEnumerable(Of List(Of String))
        Dim enumerators = sequences.Select(Function(seq) seq.GetEnumerator()).ToArray()
        Dim values = New List(Of String)(enumerators.Length)

        While True
            values.Clear()

            For i = 0 To enumerators.Length - 1
                Dim enumerator = enumerators(i)
                If Not enumerator.MoveNext() Then
                    enumerator.Reset()
                    enumerator.MoveNext()
                End If
                values.Add(enumerator.Current)
            Next

            Yield values.ToList()
        End While
    End Function
End Class
Public Class BeliefNetwork
    Public Property Nodes As List(Of Node)
    Public Sub LoadTrainingData(trainingData As Dictionary(Of String, Dictionary(Of List(Of String), Double)))
        For Each entry In trainingData
            Dim nodeName As String = entry.Key
            Dim values As Dictionary(Of List(Of String), Double) = entry.Value

            DefineCPT(nodeName, values)
        Next
    End Sub

    Public Function CreateEvidence(nodeName As String, state As String) As Dictionary(Of Node, String)
        Dim evidence As New Dictionary(Of Node, String)()
        Dim node As Node = Nodes.Find(Function(n) n.Name = nodeName)
        evidence.Add(node, state)
        Return evidence
    End Function

    Public Function GetNodeByName(nodeName As String) As Node
        Return Nodes.Find(Function(n) n.Name = nodeName)
    End Function

    Public Function PredictWithEvidence(targetNodeName As String, evidence As Dictionary(Of Node, String)) As String
        Dim targetNode As Node = GetNodeByName(targetNodeName)
        Return Predict(targetNode, evidence)
    End Function
    Public Sub LoadTrainingDataFromFile(filePath As String)
        Dim trainingData As Dictionary(Of String, Dictionary(Of List(Of String), Double)) = LoadTrainingData(filePath)
        For Each entry In trainingData
            Dim nodeName As String = entry.Key
            Dim values As Dictionary(Of List(Of String), Double) = entry.Value

            DefineCPT(nodeName, values)
        Next
    End Sub
    Public Sub ExportToFile(filePath As String)
        Dim lines As New List(Of String)()

        For Each node In Nodes
            lines.Add(node.Name)
            For Each state In node.States
                If node.Parents.Count > 0 Then
                    Dim parentStates As New List(Of String)()
                    For Each parent In node.Parents
                        parentStates.Add(parent.Name)
                    Next
                    parentStates.Add(state)
                    lines.Add(String.Join(" ", parentStates) & " " & node.CPT.Values(parentStates))
                Else
                    lines.Add(state & " " & node.CPT.Values(New List(Of String)()))
                End If
            Next
        Next

        File.WriteAllLines(filePath, lines)
        Console.WriteLine("Network exported to " & filePath)
    End Sub
    Public Sub DisplayAsTree()
        Dim form As New Form()
        Dim treeView As New TreeView()
        treeView.Dock = DockStyle.Fill
        form.Controls.Add(treeView)

        For Each node In Nodes
            Dim treeNode As New TreeNode(node.Name)

            If node.Parents.Count > 0 Then
                Dim parentNodes As New List(Of String)()
                For Each parent In node.Parents
                    parentNodes.Add(parent.Name)
                Next

                Dim parentNode As TreeNode = FindOrCreateParentNode(treeView.Nodes, parentNodes)
                parentNode.Nodes.Add(treeNode)
            Else
                treeView.Nodes.Add(treeNode)
            End If

            For Each state In node.States
                Dim stateNode As New TreeNode(state & " (" & node.CPT.Values(New List(Of String) From {state}) & ")")
                treeNode.Nodes.Add(stateNode)
            Next
        Next

        Application.Run(form)
    End Sub

    Public Shared Sub DisplayAsTree(ByRef Network As BeliefNetwork)
        Dim form As New Form()
        Dim treeView As New TreeView()
        treeView.Dock = DockStyle.Fill
        form.Controls.Add(treeView)

        For Each node In Network.Nodes
            Dim treeNode As New TreeNode(node.Name)

            If node.Parents.Count > 0 Then
                Dim parentNodes As New List(Of String)()
                For Each parent In node.Parents
                    parentNodes.Add(parent.Name)
                Next

                Dim parentNode As TreeNode = Network.FindOrCreateParentNode(treeView.Nodes, parentNodes)
                parentNode.Nodes.Add(treeNode)
            Else
                treeView.Nodes.Add(treeNode)
            End If

            For Each state In node.States
                Dim stateNode As New TreeNode(state & " (" & node.CPT.Values(New List(Of String) From {state}) & ")")
                treeNode.Nodes.Add(stateNode)
            Next
        Next

        Application.Run(form)
    End Sub


    Private Function FindOrCreateParentNode(collection As TreeNodeCollection, parentNodes As List(Of String)) As TreeNode
        Dim parentNode As TreeNode = Nothing

        For Each parentName In parentNodes
            Dim node As TreeNode = collection.Find(parentName, False).FirstOrDefault()

            If node IsNot Nothing Then
                collection = node.Nodes
                parentNode = node
            Else
                Dim newNode As New TreeNode(parentName)
                If parentNode Is Nothing Then
                    collection.Add(newNode)
                Else
                    parentNode.Nodes.Add(newNode)
                End If
                collection = newNode.Nodes
                parentNode = newNode
            End If
        Next

        Return parentNode
    End Function

    Public Shared Function LoadTrainingData(filePath As String) As Dictionary(Of String, Dictionary(Of List(Of String), Double))
        Dim trainingData As New Dictionary(Of String, Dictionary(Of List(Of String), Double))()

        If File.Exists(filePath) Then
            Dim lines As String() = File.ReadAllLines(filePath)
            Dim currentEntry As String = Nothing
            Dim currentCPT As New Dictionary(Of List(Of String), Double)()

            For Each line In lines
                Dim parts As String() = Split(line, " "c, StringSplitOptions.RemoveEmptyEntries)

                If parts.Length = 1 Then
                    ' Start of a new entry
                    If currentEntry IsNot Nothing Then
                        trainingData.Add(currentEntry, currentCPT)
                        currentCPT = New Dictionary(Of List(Of String), Double)()
                    End If

                    currentEntry = parts(0)
                ElseIf parts.Length = 2 Then
                    ' CPT entry
                    Dim state As String = parts(0)
                    Dim probability As Double = Double.Parse(parts(1))
                    currentCPT.Add(New List(Of String) From {state}, probability)
                ElseIf parts.Length > 2 Then
                    ' CPT entry with parent states
                    Dim states As New List(Of String)(parts.Length - 1)
                    For i As Integer = 0 To parts.Length - 2
                        states.Add(parts(i))
                    Next
                    Dim probability As Double = Double.Parse(parts(parts.Length - 1))
                    currentCPT.Add(states, probability)
                End If
            Next

            ' Add the last entry
            If currentEntry IsNot Nothing Then
                trainingData.Add(currentEntry, currentCPT)
            End If
        Else
            Console.WriteLine("Training data file not found.")
        End If

        Return trainingData
    End Function
    Public Sub New()
        Nodes = New List(Of Node)()
    End Sub

    Public Sub AddNode(node As Node)
        Nodes.Add(node)
    End Sub
    Public Function Predict(targetNode As Node, evidence As Dictionary(Of Node, String)) As String
        Dim engine As New InferenceEngine(Me)
        Dim conditionalProbability As Double = engine.CalculateConditionalProbability(targetNode, evidence(targetNode))
        Dim predictedState As String = If(conditionalProbability > 0.5, evidence(targetNode), GetOppositeState(targetNode, evidence(targetNode)))
        Return predictedState
    End Function

    Private Function GetOppositeState(node As Node, state As String) As String
        Return node.States.Find(Function(s) s <> state)
    End Function

    Public Sub DefineCPT(nodeName As String, values As Dictionary(Of List(Of String), Double))
        Dim node = Nodes.Find(Function(n) n.Name = nodeName)
        Dim cpt As New ConditionalProbabilityTable(node)
        For Each entry In values
            cpt.SetEntry(entry.Key, entry.Value)
        Next
        node.CPT = cpt
    End Sub
    Public Sub DisplayNetworkStructure()
        Console.WriteLine("Network Structure:")
        For Each node In Nodes
            Console.WriteLine("Node: " & node.Name)
            Console.WriteLine("Parents: " & String.Join(", ", node.Parents.Select(Function(parent) parent.Name)))
            Console.WriteLine("CPT:")
            For Each entry In node.CPT.Values
                Console.WriteLine("  P(" & node.Name & " = " & String.Join(", ", entry.Key) & ") = " & entry.Value)
            Next
            Console.WriteLine()
        Next
    End Sub
    Public Sub AddEdge(parentNode As Node, childNode As Node)
        childNode.Parents.Add(parentNode)
    End Sub
End Class

