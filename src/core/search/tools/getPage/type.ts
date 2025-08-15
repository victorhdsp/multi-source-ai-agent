export interface IGetService {
  getPage(url: string): Promise<string>;
}