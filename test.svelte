<script lang="ts">
 import { onMount, onDestroy } from 'svelte';

 import {javascript} from "@codemirror/lang-javascript"
 import {EditorView, basicSetup} from "codemirror"
 
 let { content = "" }  = $props();

 let editor: EditorView;
 let parent: HTMLElement;
 
 onMount(() => {
   editor = new EditorView({
     doc: content,
     parent: parent,
     extensions: [
       basicSetup,
       javascript({typescript: true})
     ]
   });
 })

 onDestroy(() => {
   editor?.destroy();
 });
</script>

<div bind:this={parent}></div>

<style>
  * {
      font-weight: bold;
  }
</style>
