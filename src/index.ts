import { callToMultiAgentUseCase } from "./dependences";

async function main() {
    console.log("Question Agent Service is running...");

    const response = await callToMultiAgentUseCase.execute("Quando Charles Darwin morreu?");
    console.log("Response from Question Agent Service:", response);
}

main();

// ```json - RESPOSTA COM INFORMAÇÃO NO RAG
// {
//   type: "answer",
//   content: "John Stuart Mill's \"Principles of Political Economy\" (1848) synthesized classical economic thought, incorporating social philosophy and addressing wealth distribution and the role of government. Key concepts include utilitarianism in economics, diminishing returns, and stationary state economy. It bridged classical economics with neoclassical approaches and introduced social considerations into economic theory.",
//   missing: [],
// }
// ```

// ```json - RESPOSTA SEM INFORMAÇÃO NO RAG
// {
//   type: "answer",
//   content: "Eu não sei. Os trechos fornecidos não contêm informações sobre a morte de Charles Darwin.",
//   missing: [ "data da morte de Charles Darwin" ],
// }
// ```
