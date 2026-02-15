<script lang="ts">
 import { onMount, onDestroy } from 'svelte';

 import {javascript} from "@codemirror/lang-javascript";
 import {EditorView, basicSetup} from "codemirror";

 let { content = "" }  = $props();

 let editor: EditorView;
 let parent: HTMLElement;

 const number = 42;
 let disabled = false;

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

{#if number > 42 && !disabled}
    Hello World
{:else}
    Foo Bar
{/if}

<div class="text-sm {number > 42 ? 'text-fg-red' : 'test2'} p-2"></div>
<div class="text-xl"></div>

<!-- Unquoted should look like string -->
<input type="checkbox" />

<button onclick={() => console.log("clicked")}>click me</button>

<a href="page/{p}">page {p}</a>

<!-- HTML expressions with javascript -->
<button disabled={number !== 42}>...</button>

<input required={false} placeholder="This input field is not required" />
<button {disabled}>...</button>

<!-- Turn on prettify-symbols-mode to see representation of & symbol for html entity.
     Can tell difference, because it is color of font-lock-constant-face, not text.
-->
<div>tom &amp; jerry</div>

<style>
  * {
      font-weight: bold;
  }
</style>
